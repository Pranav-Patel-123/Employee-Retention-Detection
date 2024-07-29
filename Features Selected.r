1. CFS
selected_features <- c("Age", "BusinessTravel", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "MonthlyIncome", "OverTime", "StockOptionLevel", "YearsAtCompany", "YearsWithCurrManager")

2. ChiSquare
selected_features <- c("MonthlyRate", "MonthlyIncome", "DailyRate", "Age", "TotalWorkingYears", "JobRole", "YearsAtCompany", "OverTime", "HourlyRate", "JobLevel")

3. Lasso
selected_features <- c("Age", "BusinessTravel", "DailyRate", "Department", "DistanceFromHome", "Education", "EducationField")

4. InformationGain 
selected_features <- c("Age", "Department", "BusinessTravel", "DistanceFromHome", "EducationField", "EnvironmentSatisfaction", "Gender", "HourlyRate", "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked", "OverTime", "StockOptionLevel", "TotalWorkingYears", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole", "YearsWithCurrManager")

5. genetic algorithm
selected_features <- c("Gender", "JobSatisfaction", "MonthlyRate", "NumCompaniesWorked", "RelationshipSatisfaction")


6. RandomForest
selected_features <- c("MonthlyIncome", "JobRole", "OverTime", "Age", "DailyRate", "HourlyRate", "MonthlyRate", "TotalWorkingYears", "YearsAtCompany", "DistanceFromHome")

7. RFE
selected_features <- c("OverTime", "JobRole", "HourlyRate", "DailyRate", "MonthlyRate", "MonthlyIncome", "Age", "PercentSalaryHike", "JobSatisfaction", "DistanceFromHome", "StockOptionLevel", "EducationField", "NumCompaniesWorked", "EnvironmentSatisfaction", "RelationshipSatisfaction", "Education", "WorkLifeBalance", "TrainingTimesLastYear", "TotalWorkingYears", "YearsAtCompany", "YearsWithCurrManager", "MaritalStatus", "JobInvolvement", "YearsSinceLastPromotion", "BusinessTravel", "YearsInCurrentRole", "JobLevel", "Gender")


[, c(selected_features, "Attrition")]

# Use the selected_features
selected_features <- c("MonthlyRate", "MonthlyIncome", "DailyRate", "Age", "TotalWorkingYears", "JobRole", "YearsAtCompany", "OverTime", "HourlyRate", "JobLevel")

