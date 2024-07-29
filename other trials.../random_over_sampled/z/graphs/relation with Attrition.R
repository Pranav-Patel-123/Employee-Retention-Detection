# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Normalize the numeric columns
normalize_data <- function(data) {
  numeric_columns <- select(data, where(is.numeric))
  normalized_numeric <- numeric_columns %>%
    mutate(across(everything(), ~ scale(.)))
  normalized_data <- bind_cols(select(data, -where(is.numeric)), normalized_numeric)
  return(normalized_data)
}

# Visualize relationships with the output "Attrition"
visualize_relationships <- function(data, target_variable) {
  # Normalize the data
  normalized_data <- normalize_data(data)
  
  # Get the names of all columns except the target variable
  predictors <- setdiff(names(normalized_data), target_variable)
  
  # Iterate over all predictors and plot their relationships with the target variable
  for (predictor in predictors) {
    if (predictor != target_variable) {
      if (is.factor(normalized_data[[predictor]])) {
        # For categorical variables, create a bar plot
        print(ggplot(normalized_data, aes(x = !!sym(predictor), fill = !!sym(target_variable))) +
                geom_bar(position = "dodge") +
                labs(title = paste("Bar plot of", predictor, "by", target_variable)))
      } else {
        # For continuous variables, create a density plot
        print(ggplot(normalized_data, aes(x = !!sym(predictor), fill = !!sym(target_variable))) +
                geom_density(alpha = 0.5) +
                labs(title = paste("Density plot of", predictor, "by", target_variable)))
      }
    }
  }
}

# Call the function to visualize relationships with the output "Attrition"
visualize_relationships(data, "Attrition")
