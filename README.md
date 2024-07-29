# Employee-Retention-Detection

Flow of the Project:
1. Dataset obtained from Kaggle.
2. Analysed the data and filter the data from the dataset.
3. Identify the N/A values if any (if N/A values are present then perform the imputation methods)
4. Perform class imbalance handling methods if required.
5. Split/Partition the data into Training and Testing Sets.
6. Implement the training algorithm.
7. (Optional) Use Train Control for the above trainig algorithm.
8. Perform hyper-paramater tuning.
9. Implement feature Selection for more optimised results.
10. Predict the results from the testing set.
11. Calculate and obtain the model evalution paramters such as Accuracy, precision,....
12. Draw the results in tabular format.

<html>
   <body>
      <br>
   </body>
</html>

Class imbalance handling methods used in the project are:
1. Random over sampling
2. Random under sampling
3. Ovum over sampling
4. Ovum under sampling
5. ROSE

<html>
   <body>
      <br>
   </body>
</html>

Algorithms Implemented in the project are:
1. Decision Tree
2. Logistic Regression
3. Random Forest
4. SVM
5. kNN
6. Naive Bayes

<html>
   <body>
      <br>
   </body>
</html>

Dataset/Data Splitting methods used for each datasets are:
1. 75% training and 25% testing
2. 80% training and 20% testing
3. 70% training and 30% testing
4. K-Fold
5. K-Fold within 75% training and 25% testing

<html>
   <body>
      <br>
   </body>
</html>

Feature Selection methods implemented for each mentined algorithms are:
1. Correlation-based Feature Selection (CFS)
2. Information Gain
3. Random Forest based importance score
4. Backward Elimination
5. Least Absolute Shrinkage and Selection Operator (LASSO)
6. Recursive Feature Elimination (RFE)
7. Chi Square

#

<html>
   <body>
      <h3>File Structure</h3>
   </body>
</html>

#

1. Over sampled and both sampled folders contains all the algorithms applied in the above project with the sub-folders (named after the algorithm implemented for the sub sequent files).

"DetailedReport.docx" and "Detailed Report.pdf" contains all the minor details and results (parameters: Accuracy, Precision, Specificity and Sensitivity) after applying various algorithms (such as logistic regression, decision tree, etc) and feature selection techniques with combination of various data splits.

It is important to read the "Dataset Info.txt" file is contains most valueable results drawn from the original dataset and subsequent datasets obtained after class imbalance handling.

#

<html>
   <body>
      <h3>Scripts Folder</h3>
   </body>
</html>

#

This folder contains the folder named as:
1. Decision Tree
2. Logistic Regression
3. Random Forest
4. SVM
5. kNN
6. Naive Bayes

These folders contain files named after the name of the feature selction technique used..
   
#
Q) What is Class imbalance handling ?

Ans. In simple words Class imbalance handling is the ways to increase or decrease the rows or data so that the output number of 'Y' values of "yes" and "no" become nearly similar to get unbiased results.
<html>
   <body>
      <br>
   </body>
</html>

Q) How unbiased results ?

Ans. If the model is trained on the data where the output is maximum number of times "no" then the predicted output for "yes" may be predicted sometimes inaccurate or incorrect. Similarily is when the model is trained on the data where the output is maximum number of times "yes". So, it is recommended to perform class imbalance handling in such cases to get unbiased results.
#
