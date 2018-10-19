---
title: "Tidy_README.MD"
author: "AnDahlia"
date: "19 Oktober 2018"
---
\`\`\`{r setup, include=FALSE} knitr::opts\_chunk$set(echo = TRUE)


------------------------------------------------------------------------

This is a documentation for the analysis done in the R-Script 'RUN_ANALYSIS.R'

Analysis result is a text-file 'tidy_DATAFRAME.txt'

A description of the variables there one finds in the pdf-file 'tidy_CODEBOOK.pdf'

------------------------------------------------------------------------

Aim: creation a tidy dataframe as a summary of "Human Activity Recognition Using Smartphones dataset",
Version 1.0, by Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.

Also look at a publication by Davide Anguita, Alessandro Ghio, Luca Oneto, 
Xavier Parra and Jorge L. Reyes-Ortiz, 
Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly
Support Vector Machine. International Workshop of Ambient Assisted 
Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

------------------------------------------------------------------------

the analysis uses following files from dataset above:

'features.txt': List of all features.

'activity\_labels.txt': Links the class labels with their activity name.

'train/X\_train.txt': Training set. 'train/y\_train.txt': Training labels.

'train/subject\_train.txt: 'test/X\_test.txt': Test set

'test/y\_test.txt': Test labels.feature.txt 

'test/subject\_test.txt': Each row identifies the subject
who performed the activity for each window sample.
Its range is from 1 to 30.

Note: Features are normalized and bounded within the rani -1 and +1

Note: Each feature vector is a row on the text file.

------------------------------------------------------------------------

the analysis consists of a general preparation and 5 steps\`

\# STEP 0 Preparation
=====================

cleans workspace and console, loads libraries, sets working directory

\# STEP 1 Merge of the training and test sets
=============================================

loads text files 'features.txt' and 'activity\_labels.txt' stores information 
about variable names found in 'features.txt' in a vector
creates for the each of the two groups "test" and "train" a dataframe
by using the information in 'subject\_test.txt', 'x\_test.txt' and 'y\_test.txt'
and in analogy 'subject\_train.txt', 'x\_train.txt' and 'y\_train.txt' are used.
With the vector from above one can add already descriptive variable names.
Finally both these two dataframes are put together.

result: dataframe 'df\_step1' (10299 obs, 563 variables)

\# STEP 2 Extract mean value & standard deviation for each measurement.
=======================================================================

by using the information about the variable names found in 'features.txt'
once more one creates a vector with the elements 1 or 0
1 = name of variable contains string "mean()" or "std()"
0 = name of variable doesn't contain string "mean()" or "std()" 
when element is 1 the corresponding column in dataframe "df\_step1" is kept
exception: key variables/columns "subject\_no" and "activity\_no" are kept also

result: dataframe 'df\_step2' (10299 obs, 68 variables)

\# STEP 3 Use descriptive activity names
========================================

to name the activities in the data set with the information from the file 'activity\_label.txt'
one can replace the column 'activity\_no' with the column 'activity\_label'

result: dataframe 'df\_step3' (10299 obs, 68 variables)

\# STEP 4 Appropriate labels
============================

for the data set with descriptive variable names. this step is already done in step 1

result: dataframe 'df\_step4' (equivalent to 'df\_step3')

\# STEP 5 Create independent tidy data set
==========================================

'df\_step4' ist grouped by key variables for activity and subject
then you can make a summary by computing the average of each variable 
(e.g. standard deviation of measurement of 'BodyGyroJerk')
for each activity (e.g. "STANDING") and 
each subject (e.g. participant no. 7)

result: dataframe 'df\_step5 (180 obs, 68 variables) and

result: corresponding external textfile 'tidy_dataframe.txt' 

