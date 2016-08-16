###############################
##Script to Create a Tidy data from the Propose Exercise in
##GETTING AND CLEANING DATA
## Download/Read/Convert Data
###############################
# Loading the packages
library(data.table)
library(dplyr)
# naming the url to download
filetodownload <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
setwd('getcleandata')
# Verifying if the file exists before download it
if (!file.exists('./UCI HAR Dataset.zip')){
     download.file(filetodownload, './UCI HAR Dataset.zip', mode = 'wb')
     unzip("UCI HAR Dataset.zip", exdir="C:\\Dev\\R\\GetCleanData")}
# Reading and Converting Data
#####################################################
  features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
  features <- as.character(features[,2])
  dataset_train_x <- read.table('.//UCI HAR Dataset/train/X_train.txt')
  dataset_train_activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
  dataset_train_subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt', header = FALSE, sep = ' ')
  dataset_train <- data.frame(dataset_train_subject, dataset_train_activity, dataset_train_x)
  names(dataset_train) <- c(c('subject', 'activity'), features)
  dataset_test_x <- read.table('./UCI HAR Dataset/test/X_test.txt')
  dataset_test_activity <- read.csv('./UCI HAR Dataset/test/Y_test.txt', header = FALSE, sep = ' ')
  dataset_test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
  dataset_test <- data.frame(dataset_test_subject, dataset_test_activity, dataset_test_x)
  names(dataset_test) <- c(c('subject', 'activity'), features)
# End of Reading and Converting Data
######################################################
##  Merges de two sets ( Training and Test ) to consolidate in one data set
  dataset_all <- rbind(dataset_train, dataset_test)
######################################################
## Extracts only the measurements on the mean and standard deviation for each measurement
  col_sel <- grep('mean|std', features)
  dataset_sub <- dataset_all[,c(1,2,col_sel + 2)]
######################################################
## Uses descriptive activity names to name the activities in the data set
  activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
  activity_labels <- as.character(activity_labels[,2])
  dataset_sub$activity.labels[dataset_sub$activity]
######################################################
## Appropriately labels the data set with descriptive variable names
  New_lable <- names(dataset_sub)
  New_lable <- gsub("[(][)]", "", New_lable)
  New_lable <- gsub("^t", "DominiodoTempo", New_lable)
  New_lable <- gsub("^f", "DominiodaFrequencia", New_lable)
  New_lable <- gsub("Acc", "Acelerometro", New_lable)
  New_lable <- gsub("Gyro", "Giroscopio", New_lable)
  New_lable <- gsub("Mag", "Magnitude", New_lable)
  New_lable <- gsub("-mean-", "Media", New_lable)
  New_lable <- gsub("-std-", "DesvioPadrao", New_lable)
  New_lable <- gsub("-", "_", New_lable)
  names(dataset_sub) <- New_lable
  ######################################################
  ## Creates a second, independent tidy data set with the
  ## average of each variable for each activity and each subject  
  dataset_tidy <- aggregate(dataset_sub[,3:81], by = list(activity = dataset_sub$activity, subject = dataset_sub$subject),FUN = mean)
  write.table(x = dataset_tidy, file = "data_tidy.txt", row.names = FALSE)
  
  ######END####