# Load the required libraries
library(caTools)
library(GA)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Define fitness function
fitness <- function(features) {
  # Extract the indices of selected features
  selected_indices <- which(features == 1)
  
  # Check if any features are selected
  if (length(selected_indices) == 0) {
    return(NA) # Return NA if no features are selected
  }
  
  # Extract selected features including 'Attrition'
  selected_features <- c("Attrition", names(data)[selected_indices])
  
  # Train logistic regression model using selected features
  model <- glm(Attrition ~ ., data = data[selected_features], family = "binomial")
  
  # Return the negative log-likelihood as the fitness value
  return(-logLik(model))
}

# Perform genetic algorithm feature selection
ga_result <- ga(type = "binary", fitness = fitness, nBits = ncol(data) - 1, maxiter = 50, run = 100)

# Extract selected features
selected_features <- names(data)[which(as.logical(ga_result@solution))]

# Print selected features
if (length(selected_features) > 0) {
  cat("Selected features based on Genetic Algorithm (GA):\n", selected_features, "\n")
} else {
  cat("No features selected\n")
}
