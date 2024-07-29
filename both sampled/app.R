# Load necessary libraries
library(shiny)
library(caTools)
library(rpart)
library(caret)

# Load the dataset
data <- read.csv("bothsampled.csv")

# Define categorical columns
categorical_columns <- c("Department", "BusinessTravel", "EducationField", "Gender", "JobRole", "MaritalStatus", "OverTime")

# Convert "Yes" to 1 and "No" to 0 in the Attrition column
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert specified categorical columns to factors
data[, c("Attrition", categorical_columns)] <- lapply(data[, c("Attrition", categorical_columns)], as.factor)

# Identify outliers using Z-score method
outliers <- which(abs(scale(data[, sapply(data, is.numeric)])) > 3, arr.ind = TRUE)

# Remove outliers if found
if (length(outliers) > 0) {
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

# Split the data into training (70%) and testing (30%) sets
split <- sample.split(data$Attrition, SplitRatio = 0.70)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Define selected features
selected_features <- c("OverTime", "JobRole", "HourlyRate", "DailyRate", "MonthlyRate", "MonthlyIncome", 
                       "Age", "PercentSalaryHike", "JobSatisfaction", "DistanceFromHome", "StockOptionLevel", 
                       "EducationField", "NumCompaniesWorked", "EnvironmentSatisfaction", "RelationshipSatisfaction", 
                       "Education", "WorkLifeBalance", "TrainingTimesLastYear", "TotalWorkingYears", "YearsAtCompany", 
                       "YearsWithCurrManager", "MaritalStatus", "JobInvolvement", "YearsSinceLastPromotion", 
                       "BusinessTravel", "YearsInCurrentRole", "JobLevel", "Gender")

# Define the hyperparameter grid
tuning_grid <- expand.grid(
  .degree = 2:5,
  .scale = c(0.1, 1, 10),
  .C = c(0.1, 1, 10)
)

# Train the SVM model
svm_model <- train(Attrition ~ ., 
                   data = train_data[, c(selected_features, "Attrition")], 
                   method = "svmPoly", 
                   trControl = trainControl(method = "cv", number = 10), 
                   tuneGrid = tuning_grid)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Custom Fonts */
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap');

      /* Form Container Styling */
      .form-container {
        width: 60%;
        margin: 0 auto;
        padding: 40px;
        background-color: #f5f5f5; /* Light gray */
        border-radius: 15px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        font-family: 'Roboto', sans-serif;
      }

      /* Form Header Styling */
      .form-header {
        color: #333; /* Dark gray */
        font-size: 28px;
        font-weight: 500;
        text-align: center;
        margin-bottom: 30px;
      }

      /* Form Section Styling */
      .form-section {
        margin-bottom: 30px;
      }

      /* Label Styling */
      .form-label {
        color: #555; /* Gray */
        font-weight: 500;
        margin-bottom: 8px;
      }

      /* Submit Button Styling */
      .submit-button {
        background-color: #4CAF50; /* Green */
        color: white;
        padding: 12px 24px;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        transition: background-color 0.3s ease;
      }
      .submit-button:hover {
        background-color: #45a049; /* Darker green */
      }
    "))
  ),
  fluidRow(
    column(12, align = "center",
           div(class = "form-container",
               div(class = "form-header", "Employee Attrition Prediction"),
               # Personal Information Inputs
               div(class = "form-section",
                   tags$label(class = "form-label", "Personal Information"),
                   numericInput("Age", "Age", value = 30, min = 18, max = 65),
                   selectInput("Gender", "Gender", choices = c("Male", "Female")),
                   selectInput("MaritalStatus", "Marital Status", choices = c("Single", "Married", "Divorced"))
               ),
               # Job Related Inputs
               div(class = "form-section",
                   tags$label(class = "form-label", "Job Related"),
                   selectInput("Department", "Department", choices = c("Research & Development", "Sales", "Human Resources")),
                   selectInput("JobRole", "Job Role", choices = c("Sales Executive", "Research Scientist", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative", "Manager", "Sales Representative", "Research Director", "Human Resources")),
                   numericInput("JobLevel", "Job Level", value = 1, min = 1, max = 5)
               ),
               # Work Environment Inputs
               div(class = "form-section",
                   tags$label(class = "form-label", "Work Environment"),
                   selectInput("BusinessTravel", "Business Travel", choices = c("Non-Travel", "Travel_Rarely", "Travel_Frequently")),
                   selectInput("OverTime", "Over Time", choices = c("Yes", "No")),
                   numericInput("DistanceFromHome", "Distance From Home (miles)", value = 1, min = 1, max = 30),
                   numericInput("DailyRate", "Daily Rate ($)", value = 500, min = 0),
                   numericInput("HourlyRate", "Hourly Rate ($)", value = 60, min = 0)
               ),
               # Performance Inputs
               div(class = "form-section",
                   tags$label(class = "form-label", "Performance"),
                   numericInput("PercentSalaryHike", "Percent Salary Hike (%)", value = 10, min = 0, max = 25),
                   numericInput("PerformanceRating", "Performance Rating", value = 3, min = 1, max = 5)
               ),
               # Work-life Balance Inputs
               div(class = "form-section",
                   tags$label(class = "form-label", "Work-life Balance"),
                   numericInput("TotalWorkingYears", "Total Working Years", value = 10, min = 0, max = 50),
                   numericInput("YearsAtCompany", "Years At Company", value = 5, min = 0, max = 30),
                   numericInput("YearsInCurrentRole", "Years In Current Role", value = 3, min = 0, max = 20),
                   numericInput("YearsSinceLastPromotion", "Years Since Last Promotion", value = 1, min = 0, max = 15),
                   numericInput("YearsWithCurrManager", "Years With Current Manager", value = 2, min = 0, max = 20),
                   numericInput("TrainingTimesLastYear", "Training Times Last Year", value = 2, min = 0, max = 10)
               ),
               # Submit Button
               actionButton("predict", "Predict Attrition", class = "submit-button")
           )
    )
  )
)




# Server
server <- function(input, output) {
  
  # Predict attrition
  observeEvent(input$predict, {
    new_entry <- data.frame(
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
    
    prediction <- predict(svm_model, newdata = new_entry)
    
    if (prediction == 1) {
      output$prediction <- renderText("Yes, there might be attrition.")
    } else {
      output$prediction <- renderText("No, there might not be attrition.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
