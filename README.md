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
9. (Optional) Implement feature Selection for more optimised results.
10. Predict the results from the testing set.
11. Calculate and obtain the model evalution paramters such as Accuracy, precision,....
12. Draw the results in tabular format.

#

<html>
   <body>
      <h3>Structure of the files</h3>
   </body>
</html>

#
1. Dataset folder contains all the datasets used in the project.

2. Scripts folder contains all the algorithms applied in the above project with the sub-folders (named same as the class imbalance method applied for the sub sequent files).

"Results.xlsx" contains all the results (parameters: Accuracy, Precision, Specificity and Sensitivity) after applying various algorithms such as logistic regression, decision tree, random forest, SVM and XGBoost

It is important to read the "Dataset Info.txt" file is contains most valueable results drawn from the original dataset and subsequent datasets obtained after class imbalance handling.

#

<html>
   <body>
      <h3>Scripts Folder</h3>
   </body>
</html>

#

This folder contains the folder named as:
1. random_over_sampled
2. random_under_sampled
3. rose_ovum_sample_over_sampled
4. rose_ovum_sample_over_sampled

These are named after the name of class imbalance handling methods.


Each Sub-Folder contains the Folders named as:
1. Decision Tree
2. Logistic Regression
3. Random Forest
4. SVM
5. XGBoost

These folders are named after the name of the algorithms used in the subsequent R folders/files.

Futher each folder is sub-foldered into the 3 named as:
1. 75-25
2. 75-25 & k fold
3. k fold
   
These are named after the the split techniques used in the subsequent R files.

Each folder contains 1 file with direct algorithm and 7 feature selection techniques performed on each algorithm.

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
