##########################################################
#       Course 3 - Getting and Cleaning Data Project     #
##########################################################

rm(list= ls())

# libraries

library(dplyr)
library(magrittr)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)
library(swirl)
library(XML)

setwd("/Users/luisb/Documents/Cursos/Especializacion Coursera/Getting and Cleaning Data/UCI HAR Dataset/")

# 1. Merging the training and the test sets to create one data set

  # 1.1 Reading Data Bases

    # Test

x_test = read.table("test/X_test.txt")
y_test = read.table("test/y_test.txt")
subject_test = read.table("test/subject_test.txt")

    # Train

x_train = read.table("train/X_train.txt")
y_train = read.table("train/Y_train.txt")
subject_train = read.table("train/subject_train.txt")

    # Feature and activity

features = read.table("features.txt")
activityLabels = read.table("activity_labels.txt")

  # 1.2 Assingning new names to columns in each data base

colnames(x_test) = features$V2
colnames(y_test) = "activity_id"
colnames(subject_test) = "subject_id"

colnames(x_train) = features$V2
colnames(y_train) = "activity_id"
colnames(subject_train) = "subject_id"


colnames(activityLabels) = c("activity_id", "activity_name")

  # 1.3 Merging the Data in one set

merged_test = cbind(y_test, subject_test, x_test)
merged_train = cbind(y_train, subject_train, x_train)
master_Data = rbind(merged_test, merged_train)

rm(list = setdiff(ls(), "master_Data"))

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

columnsnames = (colnames(master_Data))

mean_sd = (grepl("activity_id", columnsnames)|
             grepl("subject_id", columnsnames)|
             grepl("mean..", columnsnames)|
             grepl("std..", columnsnames))

master_mean_sd = master_Data[ , mean_sd == TRUE]

rm(list = setdiff(ls(), "master_mean_sd"))

# 3. Using descriptive activity names to name the activities in the data set

activityLabels = read.table("activity_labels.txt")
colnames(activityLabels) = c("activity_id", "activity_name")

master_mean_sd %<>% left_join(activityLabels)%>% select(activity_id,activity_name,subject_id, everything())

# 4. Appropriately labels the data set with descriptive variable names.

# Done ;)

# 5. From the data set in step 4, creates a second, independent tidy data 
#    set with the average of each variable for each activity and each subject


TidyDataSet <- aggregate(. ~subject_id + activity_id, master_mean_sd, mean)
TidyDataSet <- TidyDataSet[order(TidyDataSet$subject_id, TidyDataSet$activity_id),]

write.table(TidyDataSet, "TidyDataSet.txt", row.name=FALSE)

