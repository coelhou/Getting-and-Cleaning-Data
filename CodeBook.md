---
title: "Getting and Cleaning Data - Human Activity Recognition"
output: html_document
---

This code book describes the variables, and the transformations performed to clean up the data. 
The raw data has been downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
The resulting processed data is located in the `final_data.txt` file, a text file with the variable names and space-separated values.
Transformation operations follow instructions available in `run_analysis.R` script

## Variables 
* `activity` 
identifies 6 activity labels that may be performed by a subject:
  * `WALKING`            
  * `WALKING UPSTAIRS`   
  * `WALKING DOWNSTAIRS` 
  * `SITTING`
  * `STANDING`
  * `LAYING`  
  
**Data type**: factor  

* `subject` 
An identifier of the volunteer who carried out the experiment.  

**Data type**: integer, ranging from 1 to 30  

# Feature variables 
The selected features come from the accelerometer and gyroscope and are obtained by averaging the raw signals in a signal window sample.  

**Data type**: float, normalized and bounded within [-1,1].  

Below is the list of measurement variables:  

## Time-domain features  

  * The average body linear acceleration in the three directions X, Y and Z:  
  
`TimeBodyAccelerometerMeanX`                              
`TimeBodyAccelerometerMeanY`                              
`TimeBodyAccelerometerMeanZ`   

  * The standard deviation of the body linear acceleration in the three directions X, Y and Z:  

`TimeBodyAccelerometerStandardDeviationX`                 
`TimeBodyAccelerometerStandardDeviationY`                 
`TimeBodyAccelerometerStandardDeviationZ`  

  * The average gravity acceleration in the three directions X, Y and Z:  

`TimeGravityAccelerometerMeanX`                           
`TimeGravityAccelerometerMeanY`                           
`TimeGravityAccelerometerMeanZ`  

  * The standard deviation of the gravity acceleration in the three directions X, Y and Z:  

`TimeGravityAccelerometerStandardDeviationX`              
`TimeGravityAccelerometerStandardDeviationY`              
`TimeGravityAccelerometerStandardDeviationZ`  

  * The average body acceleration jerk in the three directions X, Y and Z:  

`TimeBodyAccelerometerJerkMeanX`                          
`TimeBodyAccelerometerJerkMeanY`                          
`TimeBodyAccelerometerJerkMeanZ`  

  * The standard deviation of body acceleration jerk in the three directions X, Y and Z:  
  
`TimeBodyAccelerometerJerkStandardDeviationX`             
`TimeBodyAccelerometerJerkStandardDeviationY`             
`TimeBodyAccelerometerJerkStandardDeviationZ`

  * The average body angular velocity in the X, Y and Z directions:  
  
`TimeBodyGyroscopeMeanX`                                  
`TimeBodyGyroscopeMeanY`                                  
`TimeBodyGyroscopeMeanZ`  

  * The standard deviation of the body angular velocity in the X, Y and Z directions:  

`TimeBodyGyroscopeStandardDeviationX`                     
`TimeBodyGyroscopeStandardDeviationY`                     
`TimeBodyGyroscopeStandardDeviationZ`  

  * The average body angular velocity jerk in the X, Y and Z directions:  
  
`TimeBodyGyroscopeJerkMeanX`                              
`TimeBodyGyroscopeJerkMeanY`                              
`TimeBodyGyroscopeJerkMeanZ`  

  * The standard deviation of the body angular velocity jerk in the X, Y and Z directions:  

`TimeBodyGyroscopeJerkStandardDeviationX`                 
`TimeBodyGyroscopeJerkStandardDeviationY`                 
`TimeBodyGyroscopeJerkStandardDeviationZ`  
   
  * The Average of the magnitude of body acceleration:  

`TimeBodyAccelerometerMagnitudeMean`  

  * The standard deviation of the magnitude of body acceleration:  

`TimeBodyAccelerometerMagnitudeStandardDeviation`  

  * The average of the magnitude of gravity acceleration:  

`TimeGravityAccelerometerMagnitudeMean`  

  * The standard deviation of the magnitude of gravity acceleration:  

`TimeGravityAccelerometerMagnitudeStandardDeviation`  

  * The average of the magnitude of body acceleration jerk:  
 
`TimeBodyAccelerometerJerkMagnitudeMean`   
                 
  * The standard deviation of the magnitude of body acceleration jerk:  

`TimeBodyAccelerometerJerkMagnitudeStandardDeviation`  

  * The average of the magnitude of body angular velocity:  

`TimeBodyGyroscopeMagnitudeMean`  

  * The standard deviation of the magnitude of body angular velocity:  

`TimeBodyGyroscopeMagnitudeStandardDeviation`  

  * The average of the time-domain magnitude of body angular velocity jerk  

`TimeBodyGyroscopeJerkMagnitudeMean`  

  * The standard deviation of magnitude of body angular velocity jerk  

`TimeBodyGyroscopeJerkMagnitudeStandardDeviation`

## Frequency-domain features  

  * The average of the body acceleration in the three directions X, Y and Z:  

`FrequencyBodyAccelerometerMeanX`                         
`FrequencyBodyAccelerometerMeanY`                         
`FrequencyBodyAccelerometerMeanZ`  

  * The Standard deviation of the body acceleration in the three directions X, Y and Z:  

`FrequencyBodyAccelerometerStandardDeviationX`            
`FrequencyBodyAccelerometerStandardDeviationY`            
`FrequencyBodyAccelerometerStandardDeviationZ`  

  * The average body acceleration jerk in the X, Y and Z directions:  

`FrequencyBodyAccelerometerJerkMeanX`                     
`FrequencyBodyAccelerometerJerkMeanY`                     
`FrequencyBodyAccelerometerJerkMeanZ`  

  * The Standard deviation of the acceleration jerk in the X, Y and Z directions:   

`FrequencyBodyAccelerometerJerkStandardDeviationX`        
`FrequencyBodyAccelerometerJerkStandardDeviationY`        
`FrequencyBodyAccelerometerJerkStandardDeviationZ`  

  * The average body angular velocity in the X, Y and Z directions:  

`FrequencyBodyGyroscopeMeanX`                             
`FrequencyBodyGyroscopeMeanY`                             
`FrequencyBodyGyroscopeMeanZ` 

  * The standard deviation of the body angular velocity in the X, Y and Z directions:  

`FrequencyBodyGyroscopeStandardDeviationX`                
`FrequencyBodyGyroscopeStandardDeviationY`                
`FrequencyBodyGyroscopeStandardDeviationZ`   
 
  * The average of the frequency-domain magnitude of body acceleration:  

`FrequencyBodyAccelerometerMagnitudeMean`  

  * The standard deviation of the frequency-domain magnitude of body acceleration:  

`FrequencyBodyAccelerometerMagnitudeStandardDeviation`  

  * The average of body acceleration jerk magnitude:  

`FrequencyBodyAccelerometerJerkMagnitudeMean`  

  * The standard deviation of body acceleration jerk magnitude:  

`FrequencyBodyAccelerometerJerkMagnitudeStandardDeviation`  

  * The average magnitude of body angular velocity:  

`FrequencyBodyGyroscopeMagnitudeMean`  

  * The standard deviation of the magnitude of body angular velocity:  

`FrequencyBodyGyroscopeMagnitudeStandardDeviation`  

  * The average magnitude of body angular velocity jerk:  

`FrequencyBodyGyroscopeJerkMagnitudeMean`  

  * The standard deviation magnitude of body angular velocity jerk:  

`FrequencyBodyGyroscopeJerkMagnitudeStandardDeviation`

## Transformations 

1. Creating the train data set `train_df` after reading then merging (by column) respectively `X_train.txt`, `y_train.txt` and `subject_train.txt`.
2. Creating the train data set `test_df` after reading then merging (by column) respectively `X_test.txt`, `y_test.txt` and `subject_test.txt`.
3. Merging the training and the test sets to create one data set.
4. Extracting only the measurements on the mean and standard deviation for each measurement.
5. Replacing the `activity` variable values with descriptive activity names.
6. Appropriately labeling the data set with descriptive variable names.
7. Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
