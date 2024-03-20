# Install and load necessary packages
# install.packages(c("caTools", "rpart", "caret"))

library(caTools)
library(rpart)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_under_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- as.factor(ifelse(data$Attrition == "Yes", 1, 0))

# Convert factor levels to valid R variable names
levels(data$Attrition) <- make.names(levels(data$Attrition))

# Normalize numeric columns
numeric_columns <- sapply(data, is.numeric)
data[, numeric_columns] <- lapply(data[, numeric_columns], function(x) (x - min(x)) / (max(x) - min(x)))

# Print correlation matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])
print("Correlation Matrix:")
print(cor_matrix)

# Perform feature selection based on correlation with CFS
correlated_features <- findCorrelation(cor_matrix, cutoff = 0.2)
print("Correlated Features:")
print(correlated_features)

# Select only the relevant columns
selected_features <- names(data)[-1][!names(data)[-1] %in% correlated_features]
data <- data[, c("Attrition", selected_features)]

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a train control object for k-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

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
