# Assuming you have already loaded the required libraries
# install.packages("randomForest")
library(randomForest)
d <- read.csv("Dataset.csv")
d$Attrition <- as.factor(d$Attrition)


# Assuming 'd' is your dataset with a column 'Attrition' indicating the class
d$Attrition <- as.character(d$Attrition)  # Convert 'Attrition' to character if it's a factor

# Save the dataset after the conversion
write.csv(d, "processed_dataset.csv", row.names = FALSE)

minority_attrition <- subset(d, Attrition == "Yes")
majority_attrition <- subset(d, Attrition == "No")

# Calculate class weights
total_samples <- nrow(d)
minority_samples <- nrow(minority_attrition)
majority_samples <- nrow(majority_attrition)

# Calculate class weights
minority_weight <- total_samples / (2 * minority_samples)
majority_weight <- total_samples / (2 * majority_samples)

# Create a vector of class weights
class_weights <- c("No" = majority_weight, "Yes" = minority_weight)

# Specify the class weights in the randomForest model
model <- randomForest(Attrition ~ ., data = d, classwt = class_weights)

# Now you can use the 'model' for prediction and evaluation
