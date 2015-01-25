##############################################################################
## Galaxy S device R analysis, Project for R class Getting and Cleaning Data" 
###############################################################################


#Links to use in project 
#http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

############### R SCRIPT NEEDS TO have these steps from directions #########################
#  You should create one R script called run_analysis.R that does the following. 
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names. 
# 5 From the data set in step 4, creates a second, independent tidy data set with
#  the average of each variable for each activity and each subject.


## ASSUME the user has the  UCI folder in the working directory and can easily
# download the data and unzip it from above links. 


# just a procedure to check directory
if (!file.exists("project")) {
        dir.create("project")
}

# date
dateFileread <- date()
dateFileread

install.packages ("dplyr")
library(dplyr)

####################################################
## read in all the files 
####################################################
# Note: no need to put in Inertial Signals data, isn't being used. See project FAQ thread

# read in the features table 
features <- read.table("./project/UCI HAR Dataset/features.txt")
dim(features)  # 561 x 2 (pay attn to 561)

# read in activites labels
activities <- read.table ("./project/UCI HAR Dataset/activity_labels.txt")
dim(activities)   # 6 x 2

##########
# train data
######## 

# read in X_train (note capital X as it was originally done that way )
X_train <-read.table ("./project/UCI HAR Dataset/train/X_train.txt")
dim(X_train)  # 7352 x 561  (pay attn to 561)

# read in y_train (note lower case y as it was orignially done that way )
y_train <- read.table ("./project/UCI HAR Dataset/train/y_train.txt",  col.names = "activ_code")
dim(y_train)   # 7352 x 1 

# read in subject train
subject_train <- read.table("./project/UCI HAR Dataset/train/subject_train.txt", col.names = "subj_id")
dim(subject_train) # 7352 x 1 


##########
# test data
######## 

# read in X_test (note capital X as it was originally done that way )
X_test <-read.table ("./project/UCI HAR Dataset/test/X_test.txt")
dim(X_test)   # 2947 x 561 (pay attn to 561)

# read in y_test (note lower case y as it was orignially done that way )
y_test <- read.table ("./project/UCI HAR Dataset/test/y_test.txt", col.names = "activ_code")
dim(y_test)    # 2947 x 1

# read in subject test
subject_test <- read.table("./project/UCI HAR Dataset/test/subject_test.txt", col.names = "subj_id")
dim(subject_test) # 2947 x 1 




####################################################
#1 Merges the training and the test sets to create one data set.
####################################################

# Y train and test
y_trainNDtest <- rbind (y_train, y_test)
dim(y_trainNDtest) # 10299 x1 
# X train and test
X_trainNDtest <-rbind(X_train, X_test)
dim (X_trainNDtest) # 10299 x 561
# Subject tran and test
Subject_trainNDtest <-rbind(subject_train, subject_test)
dim(Subject_trainNDtest) # 10299 x 1



####################################################
# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
####################################################

# label columns of features
names(features) <- c('feat_id', 'feat_name')
# index (position)  in which it matched  mean | standard deviation (std) 
index_features <- grep("-mean\\(\\)|-std\\(\\)", features$feat_name)
#subset the data to those index
X_trainNDtest <- X_trainNDtest[, index_features] 
 

# Replace names 
names(X_trainNDtest) <- gsub("\\(|\\)", "", (features[index_features, 2]))

head(X_trainNDtest)
dim(X_trainNDtest) # 10299 x 66

####################################################
# 3 Uses descriptive activity names to name the activities in the data set
# and 
# 4 Appropriately labels the data set with descriptive variable names. 
####################################################

colnames(activities) <- c("activ_code", "activ_name")
dim(activities) # 6 x 2 
# see reading the tables  previous section, for some column names


#fix activities to show names not codes
y_trainNDtest <- join (y_trainNDtest, activities, by="activ_code", match="first")
#subest
y_trainNDtest <- y_trainNDtest[,-1]

# checkin dimensions
dim(Subject_trainNDtest) #  10299     1
dim(y_trainNDtest) #        10299     1
dim(X_trainNDtest) #        10299    66

## add last two columns 
tidy_data <- cbind(Subject_trainNDtest, y_trainNDtest,X_trainNDtest)
head (tidy_data)
dim(tidy_data)   #           10299 x 68
str(tidy_data)
colnames(tidy_data)


## fix names more
names(tidy_data) <- gsub ('subj_id', "SubjectID", names (tidy_data))
names(tidy_data) <- gsub ('y_trainNDtest', "ActivityName", names(tidy_data))

names(tidy_data) <- gsub ('Gyro', "Gyroscope", names (tidy_data))
names(tidy_data) <- gsub ('Acc', "Acceleration", names (tidy_data))
names(tidy_data) <- gsub ('BodyBody', "Body", names (tidy_data))
names(tidy_data) <- gsub ('Gyroscopescopescope', "Gyroscope", names (tidy_data))
names(tidy_data) <- gsub ('GGyroscopescope', "Gyroscope", names (tidy_data))
names(tidy_data) <- gsub ('fBody', "FastFourtierTransformBody", names (tidy_data))
names(tidy_data) <- gsub ('tBody', "TimeDomainBody", names (tidy_data))
names(tidy_data) <- gsub ('tGravity', "TimeGravity", names (tidy_data))
names(tidy_data) <- gsub ('std', "StandardDeviation", names (tidy_data))
names(tidy_data) <- gsub ('mean', "Average", names (tidy_data))

colnames(tidy_data)
head(tidy_data,2)



write.table (tidy_data, file = "./project/tidy_data.txt")
####################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with
#  the average of each variable for each activity and each subject.
####################################################

# try aggregate by activity and subject
?aggregate
agg_data <- aggregate (.~ SubjectID +ActivityName, data=tidy_data, FUN=mean ) 
head(agg_data)

write.table (agg_data, file = "./project/tidy_data2.txt", row.name=FALSE)

