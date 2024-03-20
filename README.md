# Employee-Retention-Detection

Dataset folder contains all the datasets used in the project.

Scripts folder contains all the algorithms applied in the above project with the sub-folders (named same as the class imbalance method applied for the sub sequent files).

Results.xlsx contains all the results (parameters: Accuracy, Precision, Specificity and Sensitivity) after applying various algorithms such as logistic regression, decision tree, random forest, SVM and XGBoost

It is important to read the "Dataset Info.txt" file is contains most valueable results drawn from the original dataset and subsequent datasets obtained after class imbalance handling.

# Scripts Folder

This folder contains the folder named as:
1. random_over_sampled
2. random_under_sampled
3. rose_ovum_sample_over_sampled
4. rose_ovum_sample_over_sampled

These are named after the name of class imbalance handling methods.

Q) What is Class imbalance handling ?

-> In simple words Class imbalance handling is the ways to increase or decrease the rows or data so that the output number of 'Y' values of "yes" and "no" become nearly similar to get unbiased results.
<br>
Q) How unbiased results ?

-> If the model is trained on the data where the output is maximum number of times "no" then the predicted output for "yes" may be predicted sometimes inaccurate or incorrect. Similarily is when the model is trained on the data where the output is maximum number of times "yes". So, it is recommended to perform class imbalance handling in such cases to get unbiased results.

Coming Back to Scripts Folder Content.......

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
