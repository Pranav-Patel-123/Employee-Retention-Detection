library(ROSE)
d <- read.csv("Dataset.csv")
d$Attrition <- as.character(d$Attrition)  # Convert 'Attrition' to character if it's a factor

# Perform undersampling using ROSE
result <- ovun.sample(Attrition ~ ., data = d, method = "over", N = 2 * sum(d$Attrition == "No"))

# Extract balanced data from the result
result1 <- result$data

# Save balanced data with undersampling to a new CSV file
write.csv(result1, file = "rose_ovum_sample_over_sampled.csv", row.names = FALSE)
