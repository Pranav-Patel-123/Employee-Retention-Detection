# Load required libraries
library(caTools)
library(caret)
library(randomForest)
library(shiny)

# Load the dataset
data <- read.csv("random_over_sampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", 
                         "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Identify outliers using Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)])) > 3, arr.ind = TRUE)

# Remove outliers if found
if (length(outliers) > 0) {
  # Remove outliers
  data <- data[-outliers, ]
}

# Manual scaling normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to numerical columns
numeric_columns <- sapply(data, is.numeric)
if (sum(numeric_columns) > 0) {
  data[, numeric_columns] <- lapply(data[, numeric_columns], normalize)
}

# Set the seed for reproducibility
set.seed(123)
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Define selected features for training
selected_features <- c("Age", "BusinessTravel", "DailyRate", "Department", 
                       "DistanceFromHome", "Education", "EducationField")

# Train the Random Forest model
model <- randomForest(Attrition ~ ., 
                      data = train_data[, c(selected_features, "Attrition")])

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("Employee Attrition Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Input fields for selected features
      numericInput("age", "Age:", value = 30, min = 18, max = 65),
      selectInput("business_travel", "Business Travel:",
                  choices = c("Non-Travel", "Travel_Rarely", "Travel_Frequently")),
      numericInput("daily_rate", "Daily Rate:", value = 500, min = 100, max = 1500),
      selectInput("department", "Department:",
                  choices = c("Human Resources", "Research & Development", "Sales")),
      numericInput("distance_from_home", "Distance From Home:", value = 5, min = 1, max = 30),
      selectInput("education", "Education Level:",
                  choices = c("Below College", "College", "Bachelor", "Master", "Doctor")),
      selectInput("education_field", "Education Field:",
                  choices = c("Human Resources", "Life Sciences", "Marketing", "Medical", 
                              "Technical Degree", "Other")),
      actionButton("predict", "Predict Attrition")
    ),
    mainPanel(
      # Output for prediction result
      h4("Attrition Prediction Result:"),
      verbatimTextOutput("prediction_result")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  # Function to make predictions
  predict_attrition <- eventReactive(input$predict, {
    # Define factor levels based on training data
    business_levels <- levels(train_data$BusinessTravel)
    department_levels <- levels(train_data$Department)
    education_levels <- levels(train_data$Education)
    education_field_levels <- levels(train_data$EducationField)
    
    # Ensure factor levels in new data match training data
    new_data <- data.frame(
      Age = as.numeric(input$age),
      BusinessTravel = factor(input$business_travel, levels = business_levels),
      DailyRate = as.numeric(input$daily_rate),
      Department = factor(input$department, levels = department_levels),
      DistanceFromHome = as.numeric(input$distance_from_home),
      Education = factor(input$education, levels = education_levels),
      EducationField = factor(input$education_field, levels = education_field_levels)
    )
    # Predict using the trained model
    prediction <- predict(model, newdata = new_data)
    return(prediction)
  })
  
  # Output prediction result
  output$prediction_result <- renderText({
    predict_attrition()
  })
}



# Run the Shiny app
shinyApp(ui = ui, server = server)
