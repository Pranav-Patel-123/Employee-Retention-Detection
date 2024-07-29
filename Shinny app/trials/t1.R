library(shiny)
library(caTools)
library(caret)

# Load the dataset
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
data <- data[sample(nrow(data)), ]

# Split the data into training (75%) and testing (25%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)

selected_features <- c("Age", "BusinessTravel", "DailyRate", "Department", "DistanceFromHome", "Education", "EducationField")

# Train the Random Forest model
model <- train(Attrition ~ ., data = train_data[, c(selected_features, "Attrition")], method = "rf")

# Define UI
ui <- fluidPage(
  titlePanel("Employee Attrition Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Input fields for features
      numericInput("age", "Age:", min = 18, max = 65, value = 30),
      selectInput("business_travel", "Business Travel:",
                  choices = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
                  selected = "Travel_Rarely"),
      numericInput("daily_rate", "Daily Rate:", min = 0, max = 2000, value = 1000),
      selectInput("department", "Department:",
                  choices = c("Sales", "Research & Development", "Human Resources"),
                  selected = "Sales"),
      numericInput("distance_from_home", "Distance From Home:", min = 1, max = 30, value = 10),
      numericInput("education", "Education:", min = 1, max = 5, value = 3),
      selectInput("education_field", "Education Field:",
                  choices = c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Other"),
                  selected = "Life Sciences"),
      actionButton("submit", "Submit")  # Submit button
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$prediction <- renderText({
    # Check if the Submit button is clicked
    req(input$submit)
    
    # Create a new data frame with user input
    new_data <- data.frame(
      Age = input$age,
      BusinessTravel = input$business_travel,
      DailyRate = input$daily_rate,
      Department = input$department,
      DistanceFromHome = input$distance_from_home,
      Education = input$education,
      EducationField = input$education_field
    )
    
    # Make prediction using the trained model
    prediction <- predict(model, newdata = new_data)
    paste("Predicted Attrition:", prediction)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
