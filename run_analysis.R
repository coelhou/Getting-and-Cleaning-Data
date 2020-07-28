# import packages
library(data.table)
library(dplyr)



############################## 1. Merging the training and the #################
############################## test sets to create one data set.################

# 1.1 downloading data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileURL, destfile = "data.zip")

if (!file.exists("UCI HAR Dataset")) { 
  unzip("data.zip") 
}

# 1.2 reading data
read_dfs <- function(X_file, Y_file, Subject_file, 
                     X_features, Y_features, s_features){
  #' creating data sets from files then binding them by columns
  X <- fread(X_file, col.names = X_features)
  Y <- fread(Y_file, col.names = Y_features)
  S <- fread(Subject_file, col.names = s_features)
  dplyr::bind_cols(S, X, Y)
}

# read the train data
x_features = fread("UCI HAR Dataset/features.txt", sep = " ", 
                   col.names = c("id", "feature"))$feature
y_features = c("activity")
s_features = c("subject")

train_df <- read_dfs("UCI HAR Dataset/train/X_train.txt", 
                     "UCI HAR Dataset/train/y_train.txt", 
                     "UCI HAR Dataset/train/subject_train.txt", 
                     x_features, y_features, s_features)

# read the test data
test_df <- read_dfs("UCI HAR Dataset/test/X_test.txt", 
                    "UCI HAR Dataset/test/y_test.txt",
                    "UCI HAR Dataset/test/subject_test.txt",
                    x_features, y_features, s_features)

# merging the training and the test sets
all_data <- dplyr::bind_rows(train_df, test_df)



############################## 2. Extracts only the measurements on the mean ### 
############################## and standard deviation for each measurement. ####

# variables to keep
meanstd_features <- grep("subject|mean\\(\\)|std\\(\\)|activity", 
                         names(all_data), value = TRUE)
meanstd_data <- dplyr::select(all_data, all_of(meanstd_features))



############################## 3. Uses descriptive activity names to ########### 
############################## name the activities in the data set. ############

activities <- gsub("_", " ", fread("UCI HAR Dataset/activity_labels.txt", 
                                   sep = " ", 
                                   col.names = c("id", "act"))$act)

meanstd_data <- meanstd_data[, activity := factor(activity, 
                                                  labels = activities)]



############################## 4. Appropriately labels the data set ############  
############################## with descriptive variable names.#################

# a function to substitute several items at at once
substitute_items <- function(X, item){
  for (i in seq_along(X)){
    item <- gsub(X[i], names(X)[i], item)
  } 
  item
} 

new_names <- substitute_items(list(Accelerometer="Acc",
                                   Body="BodyBody",
                                   Frequency="Freq",
                                   Gyroscope="Gyro",
                                   Magnitude="Mag",
                                   Mean="-mean\\(\\)-*",
                                   StandardDeviation="-std\\(\\)-*",
                                   Frequency="^f",
                                   Time="^t"
                                   ), names(meanstd_data)) 

names(meanstd_data) <- new_names



############################# 5. creates a second data set with the average of #
############################# each variable for each activity and each subject.#

final_data <- meanstd_data %>% 
  group_by(activity, subject) %>% 
  summarise_all(mean)

write.table(final_data, file="Final_data.txt", row.names=FALSE, 
            col.names = TRUE, quote = FALSE)

