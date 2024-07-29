# Load necessary packages
library(xgboost)
library(caTools)
library(caret)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Visualize the distribution of features and relationships between variables
plot_features <- function(df) {
  for (col in names(df)) {
    # Check if the column is numeric
    if (is.numeric(df[[col]])) {
      print(ggplot(df, aes_string(x = col)) +
              geom_histogram(fill = "lightblue", color = "black", bins = 20) +
              labs(title = paste("Distribution of", col)))
    } else {
      cat("Skipping non-numeric column:", col, "\n")
    }
  }
}

# Plot graphs for all possible features
plot_features(data)

# Shuffle the data
set.seed(123)
data <- data[sample(nrow(data)), ]

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Remove outliers
remove_outliers <- function(df, lower_quantile = 0.01, upper_quantile = 0.99) {
  cat("Number of rows before outlier removal:", nrow(df), "\n")
  
  numeric_columns <- setdiff(names(df), categorical_columns)
  
  numeric_df <- df %>%
    mutate(across(all_of(numeric_columns), as.numeric)) %>%
    filter(if_any(all_of(numeric_columns), ~!is.na(.) & . > quantile(., lower_quantile) & . < quantile(., upper_quantile)))
  
  cat("Number of rows after outlier removal:", nrow(numeric_df), "\n")
  
  categorical_df <- df[categorical_columns]
  
  # Check if any rows are left after filtering numeric columns
  if (nrow(numeric_df) == 0) {
    warning("All rows removed after filtering outliers in numeric columns.")
    return(NULL)
  }
  
  # Re-attach categorical columns
  df_out <- cbind(numeric_df, categorical_df)
  return(df_out)
}

# Remove outliers
data <- remove_outliers(data)

# Check if data is NULL after removing outliers
if (is.null(data)) {
  stop("No data left after removing outliers. Please review your filtering process.")
} else {
  # Continue with data analysis
}
        