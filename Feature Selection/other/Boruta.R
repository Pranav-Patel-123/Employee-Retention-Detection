# Load the required libraries
if (!requireNamespace("caTools", quietly = TRUE)) {
  install.packages("caTools")
}
if (!requireNamespace("Boruta", quietly = TRUE)) {
  install.packages("Boruta")
}
library(caTools)
library(Boruta)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Perform Boruta feature selection
tryCatch({
  boruta_result <- Boruta(Attrition ~ ., data = data, doTrace = 0)
  selected_features <- names(getSelectedAttributes(boruta_result))
  if (length(selected_features) > 0) {
    cat("Selected features based on Boruta:\n", selected_features, "\n")
  } else {
    cat("No features selected\n")
  }
}, error = function(e) {
  cat("Error occurred during Boruta feature selection:", conditionMessage(e), "\n")
})
