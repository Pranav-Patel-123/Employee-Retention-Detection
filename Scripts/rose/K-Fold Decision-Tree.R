# Install and load necessary packages
#install.packages(c("caTools", "rpart", "caret"))

library(caTools)
library(rpart)
library(caret)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("rose.csv")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- as.factor(ifelse(data$Attrition == "Yes", 1, 0))

# Convert factor levels to valid R variable names
levels(data$Attrition) <- make.names(levels(data$Attrition))

# Set the seed for reproducibility
set.seed(123)

# Create a train control object for k-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Create a decision tree model using k-fold cross-validation
model <- train(Attrition ~ ., data = data, method = "rpart", trControl = ctrl)

# Print the trained model
print(model)

# Make predictions on the test set
predictions <- predict(model, newdata = data)

# Confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(predictions, data$Attrition)
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
