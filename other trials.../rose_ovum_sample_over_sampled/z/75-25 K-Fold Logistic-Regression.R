# Install and load necessary packages
# install.packages(c("caTools", "caret"))

library(caTools)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose_ovum_sample_over_sampled.csv")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert factor levels to valid R variable names
data$Attrition <- as.factor(data$Attrition)
levels(data$Attrition) <- make.names(levels(data$Attrition))

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a train control object for k-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Create a logistic regression model using k-fold cross-validation
model <- train(Attrition ~ ., data = train_data, method = "glm", trControl = ctrl, family = "binomial")

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
