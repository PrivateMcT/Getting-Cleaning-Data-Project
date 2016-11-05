#############################################################
##  Peer graded assignement for Getting and Cleaning Data  ##
##                                                         ##
##  Michael Thompson     02-Nov-2016                       ##
##                                                         ##
##  run_analysis.R                                         ##
##                                                         ##
#############################################################

run_analysis <- function() {
  
  ##Step 1: Merges the training and the test sets to create one data set.
  
  ##  train/subject_train.txt - Column of subject IDs - 7,352 rows x 1 col
  ##  test/subject_test.txt - Column of subject IDs - 2,947 rows x 1 col
  
  ##  train/x_train.txt - Measurement data - 7,352 rows x 561 cols
  ##  test/x_test.txt - Measurement data - 2,947 rows x 561 cols
  
  ##  train/y_train.txt - Column of activity IDs - 7,352 rows x 1 col
  ##  test/y_test.txt - Column of activity IDs - 2,947 rows x 1 col
  
  ##  features.txt - Labels for the 561 cols of measurement data - 1 row x 561 cols
  ##  activity_labels.txt - Lookup for the activity IDs - 6 entries x 2 cols
  
  ##download data file, unzip to single file 
  if(!file.exists("./data")){dir.create("./data")}
  download.file.method = "curl"
  URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(URL, destfile="./data/samsung.zip")
  unzip("./data/samsung.zip", junkpaths = TRUE, exdir = "./data/samsung")
  
  ##Read txt files into data frames
  x_train <-  read.table("./data/samsung/X_train.txt", header = FALSE)
  y_train <- read.table("./data/samsung/y_train.txt", header = FALSE)
  subject_train <- read.table("./data/samsung/subject_train.txt", header = FALSE)
  x_test <- read.table("./data/samsung/X_test.txt", header = FALSE)
  y_test <- read.table("./data/samsung/y_test.txt", header = FALSE)
  subject_test <- read.table("./data/samsung/subject_test.txt", header = FALSE)
  activity <- read.table("./data/samsung/activity_labels.txt", header = FALSE)
  features <- read.table("./data/samsung/features.txt", header = FALSE)
  
  ##Need to check if dlpyr and tidyr is installed, if not then install.
  dplyr_check <- require("tidyr")
  ifelse (dplyr_check == FALSE, install.packages("dplyr"), library(dplyr))
  tidyr_check <- require("tidyr")
  ifelse (tidyr_check == FALSE, install.packages("tidyr"), library(tidyr))
  
  ##Combine train and test sets
  ##Combine rows of measurements
  x_data <- bind_rows(x_train, x_test)
  ##Combine rows of activities
  y_data <- bind_rows(y_train, y_test)
  ##Combine rows of subject IDs
  subject_data <- bind_rows(subject_train, subject_test)
  ##Transpose column of measurement labels into row
  features <-t(features)
  ##Appy measurement labels to measurements data
  names(x_data)<-features[2,]
  
  ##Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
  ## Use pattern matching on the features labels to create a vector for the columns which end in either mean() or std() - vectorColsNeeded
  
  vectorColsNeeded <- grep("mean|std", names(x_data))
  x_data_mean_std <- x_data[,vectorColsNeeded]
  vectorColsToLose <- grep("meanFreq()", names(x_data_mean_std))
  x_data_mean_std <- select(x_data_mean_std, -vectorColsToLose)
  
  ##Step 3: Uses descriptive activity names to name the activities in the data set
  
  
  names(y_data) <- "activityID"
  names(activity)[1] <- "activityID"
  
  y_data <- left_join(y_data, activity, by="activityID")[, 2]
  x_data_mean_std <- cbind(y_data, x_data_mean_std)
  names(x_data_mean_std)[1] <- "activity"
  x_data_mean_std <- cbind(subject_data, x_data_mean_std)
  names(x_data_mean_std)[1] <- "subject_identifier"
  
  ##Step 4: Appropriately labels the data set with descriptive variable names. 
  
  names(x_data_mean_std) <- sub("tB", "time_b", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("tG", "time_g", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("fB", "frequency_b", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("Acc", "accelerometer_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("Gyro", "gyroscope_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("Mag", "magnitude_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("J", "j", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("jerk", "jerk_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("body", "body_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("body_Body", "body_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("gravity", "gravity_", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-std\\(\\)-X", "xaxis_std", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-std\\(\\)-Y", "yaxis_std", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-std\\(\\)-Z", "zaxis_std", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-std\\(\\)", "std", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-mean\\(\\)-X", "xaxis_mean", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-mean\\(\\)-Y", "yaxis_mean", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-mean\\(\\)-Z", "zaxis_mean", names(x_data_mean_std))
  names(x_data_mean_std) <- sub("-mean\\(\\)", "mean", names(x_data_mean_std))
  
  ##Step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  tidy_set_means <- x_data_mean_std %>% group_by(subject_identifier, activity) %>% summarise_each(funs(mean))
  return(tidy_set_means)
  
}

