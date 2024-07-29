# Load the required libraries
library(caTools)
library(FactoMineR)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Remove target variable
data <- data[, -which(names(data) == "Attrition")]

# Apply PCA feature selection
pca_result <- PCA(data, scale.unit = TRUE, graph = FALSE)
selected_features <- get_eigenvalue(pca_result)$quality

# Print selected features
cat("Selected features based on PCA:\n", selected_features, "\n")
