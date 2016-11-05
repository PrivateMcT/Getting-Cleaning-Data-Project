# Final Project: Getting and Cleaning Data 
## This repository contains:

**README.md** - this file explaining the contents of the repository

**run_analysis.R** - R script that performs the following steps:
* Download data file, unzip to single file 
* Read txt files into data frames
* Check if dlpyr and tidyr is installed, if not then install.
* Combine rows for train and test sets of measurements, activity IDs, subject IDs into sets
* Apply measurement labels to measurement data set
* Use pattern matching to retain only mean and standard deviation measurements
* Combine columns of activity ID, subject IDs, mean/std measurements into data set
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names
* Create a second, independent tidy data set with the average of each variable grouped by subject and activity

**tidydata.txt** – data file containing the average of each variable grouped by subject and activity

**CodeBook.md** - code book for tidydata.txt
