## 1. Install packages

install.packages("dplyr")
install.packages("tidyr")

## 2. Load libraries

library(dplyr)
library(tidyr)

## 3. Set Working Directory

setwd("/Users/ussadethlo/Desktop/DSTR/datasciencecoursera-repo")

## 4. Download and Extract file

zip_file <- download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', 'smartphones_dataset.zip', mode = 'wb')
unzip(zipfile = "smartphones_dataset.zip", exdir = "/Users/ussadethlo/Desktop/DSTR/datasciencecoursera-repo/smartphones_dataset")

## 5. Load Activities and Features

activity_labels <- read.table("./smartphones_dataset/UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("activity_number", "activity_description"))
features <- read.table("./smartphones_dataset/UCI HAR Dataset/features.txt", header = FALSE, col.names = c("column_number", "column_description"))

## 6. Load training datasets

subject_train <- read.table("./smartphones_dataset/UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subject")
X_train <- read.table("./smartphones_dataset/UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE, col.names = features$column_description)
Y_train <- read.table("./smartphones_dataset/UCI HAR Dataset/train/Y_train.txt", sep = "", header = FALSE, col.names = "activity_number")

## 7. Load test datasets

subject_test <- read.table("./smartphones_dataset/UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subject")
X_test <- read.table("./smartphones_dataset/UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE, col.names = features$column_description)
Y_test <- read.table("./smartphones_dataset/UCI HAR Dataset/test/Y_test.txt", sep = "", header = FALSE, col.names = "activity_number")

## 8. Merge training and test sets

x_merge <- rbind(X_train, X_test)
y_merge <- rbind(Y_train, Y_test)
subject_merge <- rbind(subject_train, subject_test)

## 9. Combine merged datasets into one data frame

har_all <- cbind(subject_merge, y_merge, x_merge)

## 10. Select only measurement means and standard deviations

har_sub <- select(har_all, activity_number, subject, contains("mean"), contains("std"), -contains("meanFreq"))

har_sub <- merge(har_sub, activity_labels, by = "activity_number")

har_sub <- har_sub %>%
  select(1, 2, 76, 3:75) %>%
  arrange(subject, activity_number)

har_sub$subject <- as.factor(har_sub$subject)
har_sub$activity_number <- as.factor(har_sub$activity_number)

## 11. Make the data tidy

har_sub_long <- har_sub %>%
  gather("features", "measurement", 4:76)

## 12. Summarize by the average of each variable for each activity and each subject

har_means <- har_sub_long %>%
  group_by(subject, activity_description, features) %>%
  summarise(mean_measurement = mean(measurement)) %>%
  ungroup() %>%
  arrange(subject, activity_description, features)

## 13. Write both tidy datasets (individual and means) as a .txt file

write.table(har_sub_long, file = "har_sub_long.txt", quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)

write.table(har_means, file = "har_means.txt", quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)
