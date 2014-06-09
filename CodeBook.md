### Description of data

#### Tidy data set produced by run_analysis

* 'TidyData.txt'
 * 180 rows: One per subject (30) per activity (6)
 * 81 columns: One per feature variable representing a mean or std
  * The values are the average of that variable across all (10,299) trials from  
  the original data set

#### Original data from UCI HAR Dataset (only files used by run_analysis)

* 'activity_labels.txt'
 * 6 rows by 2 columns
 * Each row matches an activity number (e.g. - 1) to a descriptive activity  
 name (e.g. - "WALKING")

* 'features.txt'
 * 561 rows by 2 columns
 * Each row matches a feature number (e.g. - 5) to the corresponding variable  
 name (e.g. - "tBodyAcc-std()-Y")
 
* 'subject_<test OR train>.txt'
 * 1 column containing the number of the subject that each time trial  
 corresponds to
 * One row per time trial (2,947 in test and 7,352 in train)

* 'X_<test OR train>.txt'
 * 561 columns containing the calculated values for each of the 561 features
 * One row per time trial (2,947 in test and 7,352 in train)

* y_<test OR train>.txt'
 * 1 column containing the activity that was being performed by the subject  
 during each time trial
 * One row per time trial (2,947 in test and 7,352 in train)

### Steps performed by run_analysis.R

1. Read in the subject, activity (y), and feature (X) data for both the test  
and train data sets

2. Read in the features.txt file, which contains the names for the 561 columns  
of the feature data

3. From the 561 original features, identify only those representing a mean or  
standard deviation calculation by pattern matching on names that contain  
"-mean" or "-std"
 * Use the resulting list to eliminate all non-relevant columns from each of the  
 test and train feature data frames

4. Append the relative subject and activity data to both of the test and train  
feature data frames by binding them as two new columns to the left side of the  
existing data frame

5. Merge the test and train data sets by appending the rows from train under the  
rows from test (could just as easily have done this in the opposite order; for  
our purposes one doesn't make any more sense than the other)

6. Clean up the feature variable names by removing hyphens and parentheses and  
assign them as column names of the combined data frame
 * _These were intentionally left in mixed case to improve readability in the  
 resulting text file_

7. Replace numbers (1 - 6) in the Activity column with descriptive activity  
names (e.g. WALKING) from activity_labels.txt

8. Create the final, tidy data set by calculating the average for each of the  
mean and std feature variables per subject per activity

9. Write the final, tidy data set to a text file in the home directory named  
"TidyData.txt"

### Additional Notes

* The "Inertial Signals" data in both the test and train folders is not required  
by run_analysis
 * The 561 features recorded in X_test.txt and X_train.txt are variables calculated  
 from the inertial signals and for the purpose of this project, we have no need  
 to go back and look at the raw data
