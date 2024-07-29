# Load necessary libraries
library(shiny)
library(rpart)

# UI
ui <- fluidPage(
  titlePanel("Employee Attrition Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("Age", "Age"),
      selectInput("BusinessTravel", "Business Travel", choices = c("Non-Travel", "Travel_Rarely", "Travel_Frequently")),
      numericInput("DailyRate", "Daily Rate", value = 500),
      selectInput("Department", "Department", choices = c("Research & Development", "Sales", "Human Resources")),
      numericInput("DistanceFromHome", "Distance From Home", value = 1),
      numericInput("Education", "Education", value = 1),
      selectInput("EducationField", "Education Field", choices = c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Other")),
      numericInput("EnvironmentSatisfaction", "Environment Satisfaction", value = 1),
      selectInput("Gender", "Gender", choices = c("Male", "Female")),
      numericInput("HourlyRate", "Hourly Rate", value = 60),
      numericInput("JobInvolvement", "Job Involvement", value = 1),
      numericInput("JobLevel", "Job Level", value = 1),
      selectInput("JobRole", "Job Role", choices = c("Sales Executive", "Research Scientist", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative", "Manager", "Sales Representative", "Research Director", "Human Resources")),
      numericInput("JobSatisfaction", "Job Satisfaction", value = 1),
      selectInput("MaritalStatus", "Marital Status", choices = c("Single", "Married", "Divorced")),
      numericInput("MonthlyIncome", "Monthly Income", value = 2000),
      numericInput("MonthlyRate", "Monthly Rate", value = 5000),
      numericInput("NumCompaniesWorked", "Number of Companies Worked", value = 1),
      selectInput("OverTime", "Over Time", choices = c("Yes", "No")),
      numericInput("PercentSalaryHike", "Percent Salary Hike", value = 10),
      numericInput("PerformanceRating", "Performance Rating", value = 3),
      numericInput("RelationshipSatisfaction", "Relationship Satisfaction", value = 1),
      numericInput("StockOptionLevel", "Stock Option Level", value = 0),
      numericInput("TotalWorkingYears", "Total Working Years", value = 1),
      numericInput("TrainingTimesLastYear", "Training Times Last Year", value = 1),
      numericInput("WorkLifeBalance", "Work Life Balance", value = 1),
      numericInput("YearsAtCompany", "Years At Company", value = 1),
      numericInput("YearsInCurrentRole", "Years In Current Role", value = 1),
      numericInput("YearsSinceLastPromotion", "Years Since Last Promotion", value = 1),
      numericInput("YearsWithCurrManager", "Years With Current Manager", value = 1),
      
      actionButton("predict", "Predict Attrition")
    ),
    
    mainPanel(
      textOutput("prediction")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Train decision tree model
  model <- reactive({
    set.seed(123)
    data <- read.csv("random_over_sampled.csv")
    data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)
    categorical_columns <- c("Department", "BusinessTravel", "EducationField", "Gender", "JobRole", "MaritalStatus", "OverTime")
    data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)
    rpart(Attrition ~ ., data = data, method = "class")
  })
  
  # Predict attrition
  observeEvent(input$predict, {
    new_entry <- reactive({
      data.frame(
        Age = as.numeric(input$Age),
        BusinessTravel = input$BusinessTravel,
        DailyRate = as.numeric(input$DailyRate),
        Department = input$Department,
        DistanceFromHome = as.numeric(input$DistanceFromHome),
        Education = as.numeric(input$Education),
        EducationField = input$EducationField,
        EnvironmentSatisfaction = as.numeric(input$EnvironmentSatisfaction),
        Gender = input$Gender,
        HourlyRate = as.numeric(input$HourlyRate),
        JobInvolvement = as.numeric(input$JobInvolvement),
        JobLevel = as.numeric(input$JobLevel),
        JobRole = input$JobRole,
        JobSatisfaction = as.numeric(input$JobSatisfaction),
        MaritalStatus = input$MaritalStatus,
        MonthlyIncome = as.numeric(input$MonthlyIncome),
        MonthlyRate = as.numeric(input$MonthlyRate),
        NumCompaniesWorked = as.numeric(input$NumCompaniesWorked),
        OverTime = input$OverTime,
        PercentSalaryHike = as.numeric(input$PercentSalaryHike),
        PerformanceRating = as.numeric(input$PerformanceRating),
        RelationshipSatisfaction = as.numeric(input$RelationshipSatisfaction),
        StockOptionLevel = as.numeric(input$StockOptionLevel),
        TotalWorkingYears = as.numeric(input$TotalWorkingYears),
        TrainingTimesLastYear = as.numeric(input$TrainingTimesLastYear),
        WorkLifeBalance = as.numeric(input$WorkLifeBalance),
        YearsAtCompany = as.numeric(input$YearsAtCompany),
        YearsInCurrentRole = as.numeric(input$YearsInCurrentRole),
        YearsSinceLastPromotion = as.numeric(input$YearsSinceLastPromotion),
        YearsWithCurrManager = as.numeric(input$YearsWithCurrManager)
      )
    })
    
    prediction <- predict(model(), newdata = new_entry(), type = "class")
    
    if (length(prediction) > 0 && prediction == 1) {
      output$prediction <- renderText("Yes, there might be attrition.")
    } else if (length(prediction) > 0 && prediction == 0) {
      output$prediction <- renderText("No, there might not be attrition.")
    } else {
      output$prediction <- renderText("Prediction could not be made.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)