# Assuming you have already loaded the required libraries
# install.packages("randomForest")
library(randomForest)
d <- read.csv("Dataset.csv")
d$Attrition <- as.factor(d$Attrition)

minority_attrition <- subset(d, Attrition == "Yes")
majority_attrition <- subset(d, Attrition == "No")

# Calculate class weights
total_samples <- nrow(d)
minority_samples <- nrow(minority_attrition)
majority_samples <- nrow(majority_attrition)

# Calculate class weights
minority_weight <- total_samples / minority_samples
majority_weight <- total_samples / majority_samples

# Create a vector of class weights
class_weights <- c("No" = majority_weight, "Yes" = minority_weight)

# Specify the class weights in the randomForest model
model <- randomForest(Attrition ~ ., data = d, classwt = class_weights)

# Create a new dataset for predictions
new_data <- d
new_data$Predicted_Attrition <- as.factor(predict(model, newdata = new_data))

# Save the new dataset with predicted values
write.csv(new_data, "final_dataset.csv", row.names = FALSE)