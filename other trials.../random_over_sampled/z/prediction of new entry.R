# Install and load necessary packages
# install.packages("caTools")
# install.packages("rpart")

library(caTools)
library(rpart)

# Load the dataset
# Replace 'your_data.csv' with the actual name of your dataset file
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Create a decision tree model
model <- rpart(Attrition ~ ., data = train_data, method = "class")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "class")

# Confusion matrix to evaluate the model
conf_matrix <- table(predictions, test_data$Attrition)
print(conf_matrix)

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

# Hard code the first entry for input
new_entry <- data.frame(
  Age = 33,
  BusinessTravel = "Travel_Rarely",
  DailyRate = 527,
  Department = "Research & Development",
  DistanceFromHome = 1,
  Education = 4,
  EducationField = "Other",
  EnvironmentSatisfaction = 4,
  Gender = "Male",
  HourlyRate = 63,
  JobInvolvement = 3,
  JobLevel = 1,
  JobRole = "Research Scientist",
  JobSatisfaction = 4,
  MaritalStatus = "Single",
  MonthlyIncome = 2686,
  MonthlyRate = 5207,
  NumCompaniesWorked = 1,
  OverTime = "Yes",
  PercentSalaryHike = 13,
  PerformanceRating = 3,
  RelationshipSatisfaction = 3,
  StockOptionLevel = 0,
  TotalWorkingYears = 10,
  TrainingTimesLastYear = 2,
  WorkLifeBalance = 2,
  YearsAtCompany = 10,
  YearsInCurrentRole = 9,
  YearsSinceLastPromotion = 7,
  YearsWithCurrManager = 8
)


# Convert factors to proper levels
for (col in categorical_columns) {
  new_entry[[col]] <- as.factor(new_entry[[col]])
}

# Ensure numeric columns are converted to numeric and factors to character
for (col in names(data)) {
  if (col %in% names(new_entry)) {
    if (is.factor(new_entry[[col]])) {
      new_entry[[col]] <- as.character(new_entry[[col]])
    } else if (!is.numeric(new_entry[[col]])) {
      new_entry[[col]] <- as.numeric(new_entry[[col]])
    }
  } else {
    new_entry[[col]] <- NA
  }
}

# Print new entry for debugging
#print("New entry:")
#print(new_entry)

# Predict attrition for the new entry
new_prediction <- predict(model, newdata = new_entry, type = "class")

# Print the prediction
if (length(new_prediction) > 0 && new_prediction == 1) {
  print("Yes, there might be attrition.")
} else if (length(new_prediction) > 0 && new_prediction == 0) {
  print("No, there might not be attrition.")
} else {
  print("Prediction could not be made.")
}
