# Load necessary packages
library(xgboost)
library(caTools)
library(caret)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Convert specified categorical columns to factors
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Visualize the distribution of features and relationships between variables
# Example: Distribution of Age
if ("Age" %in% names(data)) {
  print(ggplot(data, aes(x = Age)) +
          geom_histogram(fill = "lightblue", color = "black", bins = 20) +
          labs(title = "Distribution of Age"))
} else {
  warning("No 'Age' column found in the dataset. Unable to plot histogram.")
}

# Example: Relationship between Age and Attrition
if ("Age" %in% names(data) && "Attrition" %in% names(data)) {
  print(ggplot(data, aes(x = Age, fill = Attrition)) +
          geom_density(alpha = 0.5) +
          labs(title = "Density Plot of Age by Attrition"))
} else {
  warning("One or both of the columns 'Age' and 'Attrition' are missing. Unable to plot density plot.")
}

# Example: Boxplot of MonthlyIncome by Attrition
if ("MonthlyIncome" %in% names(data) && "Attrition" %in% names(data)) {
  print(ggplot(data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
          geom_boxplot() +
          labs(title = "Boxplot of MonthlyIncome by Attrition"))
} else {
  warning("One or both of the columns 'MonthlyIncome' and 'Attrition' are missing. Unable to plot boxplot.")
}

# Histograms for numeric variables
numeric_columns <- setdiff(names(data), categorical_columns)

for (col in numeric_columns) {
  print(ggplot(data, aes(x = !!sym(col))) +
          geom_histogram(fill = "lightblue", color = "black", bins = 20, stat = "count") +
          labs(title = paste("Histogram of", col)))
}

# Bar plots for categorical variables
for (col in categorical_columns) {
  print(ggplot(data, aes(x = !!sym(col))) +
          geom_bar(fill = "lightblue", color = "black", stat = "count") +
          labs(title = paste("Bar plot of", col)))
}

# Shuffle the data
set.seed(123)
data <- data[sample(nrow(data)), ]

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
  
  # Split the data into training (75%) and testing (25%) sets
  if ("Attrition" %in% names(data)) {
    split <- sample.split(data$Attrition, SplitRatio = 0.75)
    train_data <- subset(data, split == TRUE)
    test_data <- subset(data, split == FALSE)
  } else {
    stop("No 'Attrition' column found in the dataset. Unable to split data.")
  }
  
  # Convert data to matrix format for XGBoost
  train_matrix <- model.matrix(Attrition ~ . - 1, data = train_data)
  test_matrix <- model.matrix(Attrition ~ . - 1, data = test_data)
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(train_data$Attrition) - 1)
  dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(test_data$Attrition) - 1)
  
  # Specify XGBoost parameters
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.1,
    max_depth = 10,  # Adjust as needed
    min_child_weight = 5,  # Adjust as needed
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train the XGBoost model
  xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)
  
  # Make predictions on the test set
  predictions <- predict(xgb_model, dtest)
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Confusion matrix to evaluate the model
  conf_matrix <- table(predicted_labels, as.numeric(test_data$Attrition) - 1)
  print(conf_matrix)
  
  # Calculate evaluation parameters
  # Calculate accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  print(paste("Accuracy:", accuracy))
  
  # Calculate sensitivity (true positive rate)
  sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  print(paste("Sensitivity:", sensitivity))
  
  # Calculate precision (positive predictive value)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  print(paste("Precision:", precision))
  
  # Calculate specificity (true negative rate)
  specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
  print(paste("Specificity:", specificity))
}