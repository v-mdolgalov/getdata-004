## This function was designed to combine, summarize, and make clean a set  
## of data collected from the embedded accelerometer and gyroscopes of Samsung  
## Galaxy S smartphones.

## The original data was collected as part of a study designed to explore the  
## possibility of human activity recognition using smartphones. A full description  
## is available at the site where the original data was obtained:
        
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

run_analysis <- function() {
        
        # Test data set: read the data
        subj_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        activity_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
        ftrData_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
        
        # Train data: read the data
        subj_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        activity_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
        ftrData_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
        
        # Load all rows (561) of the features.txt file
        # The rows in this file correspond to the columns in X_train and X_test
        features <- read.table("./UCI HAR Dataset/features.txt",
                               col.names = c("featureNumber", "featureName"))
        
        # Narrow it down to just the features that represent calculation of the
        # mean or standard deviation for each measurement
        meanStdFeatures <- grepl("-mean|-std", features$featureName)
        
        # Get rid of the unneeded feature data sets to get only the mean and standard deviation
        # measurements
        ftrData_test <- ftrData_test[ , meanStdFeatures]
        ftrData_train <- ftrData_train[ , meanStdFeatures]
        
        # Append the subject and activity columns to the featureData data frames
        data_test <- cbind(subj_test, activity_test, ftrData_test)
        data_train <- cbind(subj_train, activity_train, ftrData_train)
        
        # Merge the test and training data sets based on the common key
        data <- rbind(data_test, data_train)
        
        # Clean up variable names
        featureNames <- features[meanStdFeatures, 2]
        featureNames <- gsub("-","", featureNames)
        featureNames <- gsub("\\(\\)","", featureNames)
        
        # Assign variable names from features.txt to the columns of featureData
        colnames(data) <- c("Subject", "Activity", as.character(featureNames))
        
        # Replace activity numbers with activity names (e.g. - 1 -> WALKING)
        activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        for (i in 1:length(activity_labels[, 1])) {
                data[ , 2] <- gsub(activity_labels[i, 1], activity_labels[i, 2],
                             data[ , 2])
        }
        
        # Create a second data set with the average of each variable for
        # each activity and each subject
        tidyData <- aggregate(data[ , 3:ncol(data)], list(Subject = data[ ,1],
                                                Activity = data[ ,2]), mean)
        # Trim the additional two columns created by the aggregate function
        #tidyData <- tidyData[ , 2:ncol(tidyData)]
        
        # Write the combined and averaged tidy data set to a CSV file named
        # "TidyData.csv" in the home directory
        # Exclude row names so the data is easily pastable into Excel
        write.table(tidyData, "./TidyData.txt", row.names = FALSE)
}
