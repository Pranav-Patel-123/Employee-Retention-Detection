library(shiny)
library(randomForest)

# Define UI for application
ui <- fluidPage(
  
  titlePanel("Employee Attrition Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      # Input fields for selected features
      numericInput("age", "Age:", value = 30, min = 18, max = 70),
      selectInput("business_travel", "Business Travel:", 
                  choices = c("Travel_Rarely", "Travel_Frequently", "Non-Travel")),
      numericInput("environment_satisfaction", "Environment Satisfaction:", value = 3, min = 1, max = 4),
      numericInput("job_involvement", "Job Involvement:", value = 3, min = 1, max = 4),
      numericInput("job_level", "Job Level:", value = 2, min = 1, max = 5),
      numericInput("monthly_income", "Monthly Income:", value = 5000, min = 1000, max = 20000),
      selectInput("over_time", "Over Time:", choices = c("Yes", "No")),
      numericInput("stock_option_level", "Stock Option Level:", value = 1, min = 0, max = 3),
      numericInput("years_at_company", "Years at Company:", value = 3, min = 0, max = 40),
      numericInput("years_with_curr_manager", "Years with Current Manager:", value = 2, min = 0, max = 20),
      actionButton("predict_button", "Predict Attrition")
    ),
    
    mainPanel(
      textOutput("prediction_text")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load the trained model
  random_forest_model <- readRDS("random_forest_model.rds")
  
  # Function to preprocess input data
  preprocess_data <- reactive({
    new_data <- data.frame(
      Age = input$age,
      BusinessTravel = input$business_travel,
      EnvironmentSatisfaction = input$environment_satisfaction,
      JobInvolvement = input$job_involvement,
      JobLevel = input$job_level,
      MonthlyIncome = input$monthly_income,
      OverTime = input$over_time,
      StockOptionLevel = input$stock_option_level,
      YearsAtCompany = input$years_at_company,
      YearsWithCurrManager = input$years_with_curr_manager
    )
    
    # Convert BusinessTravel and OverTime to factors
    new_data$BusinessTravel <- as.factor(new_data$BusinessTravel)
    new_data$OverTime <- as.factor(new_data$OverTime)
    
    return(new_data)
  })
  
  # Function to make predictions
  output$prediction_text <- renderText({
    req(input$predict_button)
    
    # Preprocess input data
    new_data <- preprocess_data()
    
    # Make prediction using the trained model
    prediction <- predict(random_forest_model, newdata = new_data)
    
    # Return prediction
    paste("Predicted Attrition:", prediction)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
