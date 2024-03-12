# Install and load necessary packages
# install.packages(c("caTools", "rpart", "caret"))

library(caTools)
library(rpart)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- as.factor(ifelse(data$Attrition == "Yes", 1, 0))

# Convert factor levels to valid R variable names
levels(data$Attrition) <- make.names(levels(data$Attrition))
# Convert data to numeric for correlation calculation
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data)

# Perform feature selection based on correlation
correlated_features <- findCorrelation(cor_matrix, cutoff = 0.04)
selected_features <- names(numeric_data)[-1][!names(numeric_data)[-1] %in% correlated_features]

# Select only the relevant columns
data <- data[, c("Attrition", selected_features)]

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a train control object for k-fold cross-validation
#ctrl <- trainControl(method = "repeatedcv", number = 50, classProbs = TRUE, summaryFunction = twoClassSummary)
#ctrl <- trainControl(method = "boot", number = 500)
#ctrl <- trainControl(method = "repeatedcv", number = 50, repeats = 15)
#ctrl <- trainControl(method = "repeatedholdout", number = 10, repeats = 5)
ctrl <- trainControl(method = "LOOCV")

# Create a decision tree model using k-fold cross-validation
model <- train(Attrition ~ ., data = train_data, method = "rpart", trControl = ctrl)

# Print the trained model
print(model)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(predictions, test_data$Attrition)
print(conf_matrix)

# Calculate accuracy
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy:", accuracy))

# Calculate sensitivity (true positive rate)
sensitivity <- conf_matrix$byClass["Sensitivity"]
print(paste("Sensitivity:", sensitivity))

# Calculate precision (positive predictive value)
precision <- conf_matrix$byClass["Pos Pred Value"]
print(paste("Precision:", precision))

# Calculate specificity (true negative rate)
specificity <- conf_matrix$byClass["Neg Pred Value"]
print(paste("Specificity:", specificity))
