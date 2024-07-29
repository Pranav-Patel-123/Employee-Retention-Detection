# Load necessary packages
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Visualize the distribution of features and relationships between variables
plot_features <- function(df) {
  for (col in names(df)) {
    # Check if the column is numeric
    if (is.numeric(df[[col]])) {
      # Calculate the lower and upper quartiles
      lower_quantile <- quantile(df[[col]], 0.25)
      upper_quantile <- quantile(df[[col]], 0.75)
      
      # Calculate the interquartile range
      iqr <- upper_quantile - lower_quantile
      
      # Define the lower and upper bounds for outliers
      lower_bound <- lower_quantile - 1.5 * iqr
      upper_bound <- upper_quantile + 1.5 * iqr
      
      # Identify outliers
      outliers <- df[[col]][df[[col]] < lower_bound | df[[col]] > upper_bound]
      
      if (length(outliers) == 0) {
        cat("No outliers found for", col, "\n")
      } else {
        # Print outliers
        cat("Outliers for", col, ":", outliers, "\n")
        
        # Create box plot
        print(ggplot(df, aes_string(y = col)) +
                geom_boxplot(fill = "lightblue", color = "black") +
                geom_point(data = data.frame(x = 1, y = outliers), aes(x, y), color = "red", size = 3) +
                labs(title = paste("Boxplot of", col)))
      }
    } else {
      cat("Skipping non-numeric column:", col, "\n")
    }
  }
}

# Function to remove outliers
remove_outliers <- function(df, lower_quantile = 0.01, upper_quantile = 0.99) {
  cat("Initial number of rows:", nrow(df), "\n")
  
  for (col in names(df)) {
    # Check if the column is numeric
    if (is.numeric(df[[col]])) {
      # Calculate the lower and upper quantiles
      lower_q <- quantile(df[[col]], lower_quantile)
      upper_q <- quantile(df[[col]], upper_quantile)
      
      # Filter out rows outside the quantiles
      df <- df[df[[col]] >= lower_q & df[[col]] <= upper_q, ]
    }
  }
  
  cat("Final number of rows after outlier removal:", nrow(df), "\n")
  return(df)
}

# Plot features and remove outliers
plot_features(data)
data <- remove_outliers(data)
