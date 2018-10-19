
#RUN_ANALYSIS.R
###############################################################################
#STEP 0 preparation
###############################################################################

cat("\014") ## clear console
rm(list = ls())

library(dplyr)

#STEP 1 Merges the training and the test sets to create one data set.
###############################################################################

df_f <- read.table("features.txt", header = FALSE, col.names = c("var_no", "var_label"))
var_labels_vect <- as.vector(df_f[,2]) #vector with descriptive variable names
df_a <- read.table("activity_labels.txt", header = FALSE, col.names = c("activity_no", "activity_label"))


combine_df_yx <- function(group, df_name_s, df_name_x, df_name_y)
{
    df_s <- read.table(df_name_s,header = FALSE, col.names = c("subject_no"))
    df_x <- read.table(df_name_x,header = FALSE, col.names = var_labels_vect) #includes Step 4 (descriptive variable names)
    df_y <- read.table(df_name_y,header = FALSE, col.names = c("activity_no"))
    cbind(df_s, df_y, df_x)
}
df_yx_test <- combine_df_yx("test", "subject_test.txt", "x_test.txt", "y_test.txt")
df_yx_train <- combine_df_yx("train", "subject_train.txt", "x_train.txt", "y_train.txt")
df_step1 <- rbind(df_yx_test, df_yx_train)


#STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement.
###############################################################################################
#create new vector "ms": its elements ar "1" or "0",
#"1" if name of variable contain string "mean()" or "std()",
# "0" if name of variable doesn't contain these strings
ms <- grepl("mean()", df_f[,2], fixed = TRUE)  + grepl("std()", df_f[,2], fixed = TRUE)
# this vector is added as a column to dataframe "df_f"
df_f_mod <- cbind(df_f,ms)
#create a vector with relevant variable names
# 2 first columns = subject_no, activity_no
keep_var_vect <- c(1, 2, df_f_mod[which(df_f_mod$ms == 1) + 2 ,1])
df_step2  <- df_step1[keep_var_vect]


#STEP 3 Uses descriptive activity names to name the activities in the data set
###############################################################################################
#column 2 "activity no" from dataframe "df_mean_and_std",
#column 68 "activity_label" from dataframe "df_a"
#delete old column 2 and make column 68 new column 2 ==> 67 columns with  first columns =  subject_no, activity_label
df_step3 <- select(left_join(df_step2, df_a, by = "activity_no"),-2)[,c(1,68,2:67)]

#STEP 4 Appropriately labels the data set with descriptive variable names.
###############################################################################################
#see above Step 1
df_step4 <- df_step3

#STEP 5 From the data set in step 4, creates a second, independent tidy data set
###############################################################################################
# with the average of each variable for each activity and each subject.
df_step5 <- df_step4 %>% group_by(subject_no, activity_label)  %>% summarise_all(mean)

write.table(df_step5, file = "tidy_DATAFRAME.txt",row.names = FALSE)


