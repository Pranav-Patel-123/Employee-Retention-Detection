d <- read.csv("Dataset.csv")
# Assuming 'd' is your dataset with a column 'Attrition' indicating the class
d$Attrition <- as.character(d$Attrition)  # Convert 'Attrition' to character if it's a factor

minority_attrition <- subset(d, Attrition == "Yes")
majority_attrition <- subset(d, Attrition == "No")

# Randomly sample from the majority attrition
sampled_majority_attrition <- majority_attrition[sample(nrow(majority_attrition), nrow(minority_attrition)), ]

# Combine minority and sampled majority
balanced_data_attrition <- rbind(minority_attrition, sampled_majority_attrition)

# Save balanced data to a new CSV file
write.csv(balanced_data_attrition, file = "balanced_data_attrition.csv", row.names = FALSE)


