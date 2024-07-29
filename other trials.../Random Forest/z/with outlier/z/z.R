library(caret)




# Create a training control specifying the 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)
# Create a decision tree model using 10-fold cross-validation
model <- train(Attrition ~ ., data = train_data, method = "rpart", trControl = train_control)







# Count rows before outlier handling
rows_before <- nrow(data)
# Identify outliers using Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)])) > 3, arr.ind = TRUE)
# Remove outliers if found
if (length(outliers) > 0) {
  # Remove outliers
  data <- data[-outliers, ]} 
# Count rows after outlier handling
rows_after <- nrow(data)
# Print count of rows before and after outlier handling
cat("Rows before Z-score method of outlier handling:", rows_before, "\n")
cat("Rows after Z-score method of outlier handling:", rows_after, "\n")
# Manual scaling normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))}
# Apply normalization to numerical columns
numeric_columns <- sapply(data, is.numeric)
if (sum(numeric_columns) > 0) {
  data[, numeric_columns] <- lapply(data[, numeric_columns], normalize)
  
  cat("\nDataset normalized.\n")
} else {
  cat("\nNo numerical columns found in the dataset.\n")
}