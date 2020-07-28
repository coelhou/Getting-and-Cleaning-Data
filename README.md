# Getting and Cleaning Data
In this project, we are dealing with the data available at the Human Activity Recognition database which is built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.  

Each person performed six activities (`WALKING`, `WALKING UPSTAIRS`, `WALKING DOWNSTAIRS`, `SITTING`, `STANDING`, `LAYING`) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz are captured. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.  

For each record in the dataset it is provided:  

  * Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
  * Triaxial Angular velocity from the gyroscope.
  * A 561-feature vector with time and frequency domain variables.
  * Its activity label.
  * An identifier of the subject who carried out the experiment.  
  
This project aims to demonstrate the ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. For that matter, an R script is created to perform the following tasks:  

1. Merging the training and the test sets to create one data set.
2. Extracting only the measurements on the mean and standard deviation for each measurement.
3. Replacing the `activity` variable values with descriptive activity names.
4. Appropriately labeling the data set with descriptive variable names.
5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
