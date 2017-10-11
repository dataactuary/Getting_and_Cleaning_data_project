######################################################################################
#    
#  Overview  -  for more information check the README.MD file. 
#  This script called run_analysis.R that does the following:
#    1. Merges the training and the test sets to create one data set.
#    2. Extracts only the measurements on the mean and standard deviation for 
#       each measurement.
#    3. Uses descriptive activity names to name the activities in the data set
#    4. Appropriately labels the data set with descriptive variable names.
#    5. From the data set in step 4, creates a second, independent tidy data set with 
#       the average of each variable for each activity and each subject.
#
#######################################################################################


#####################
# Getting the data  #
#####################


# This scrpit assumes the Samsung data is available
#zipUrl   =  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#zipName  ="getdata_projectfiles_UCI HAR Dataset.zip"

#if (!file.exists(zipName)) {
#    download.file(zipUrl, zipName, mode = "wb")
#}

# unzip zip file containing data if data directory doesn't already exist

# start code here if data is already available

Datapath <- "UCI HAR Dataset"
if (!file.exists(Datapath)) {
    unzip(zipName)
}

library(dplyr)
#####################
# Reading the data  #
#####################

# read train data 
trainSubjects <- read.table(file.path(Datapath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(Datapath, "train", "X_train.txt"))
trainActivity <-read.table(file.path(Datapath, "train", "y_train.txt"))

# read test data 
testSubjects <- read.table(file.path(Datapath, "test", "subject_test.txt"))
testValues <- read.table(file.path(Datapath, "test", "X_test.txt"))
testActivity <- read.table(file.path(Datapath, "test", "y_test.txt"))

# read features and activities
features <- read.table(file.path(Datapath, "features.txt"),colClasses = "character")
activities <- read.table(file.path(Datapath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


########################################################################
# Step 1  Merge the training and the test sets to create one data set. #
########################################################################

trainData<-cbind(trainSubjects, trainActivity, trainValues)
testData<-cbind(testSubjects, testActivity, testValues)
ActivityData<-rbind(trainData, testData)


#############################################################################
# Step 2 Extract only the mean and standard deviation for each measurement. #
#############################################################################

# Assign column names
colnames(ActivityData)<-c("SubjectId", "Activity", features[ , 2])

# Keep or dicard columns
columnsToKeep <- grepl("SubjectId|Activity|mean|std", colnames(ActivityData))

# New dataset with selected columns only
ActivityData <- ActivityData[, columnsToKeep]


########################################################################
# Step 3 Use descriptive names to name the activities in the data set  #
########################################################################

ActivityData$Activity <- factor(ActivityData$Activity, levels = activities[, 1], labels = activities[, 2])

############################################################################
# Step 4 Appropriately labels the data set with descriptive variable names # 
############################################################################

# get column names
CNames <- colnames(ActivityData)

# remove special characters and expand abbreviations
CNames <- gsub("[\\(\\)-]", "", CNames)

CNames <- gsub("Acc", "Accelerometer", CNames)
CNames <- gsub("Gyro", "Gyroscope", CNames)
CNames <- gsub("Mag", "Magnitude", CNames)
CNames <- gsub("Freq", "Frequency", CNames)
CNames <- gsub("mean", "MeanValue", CNames)
CNames <- gsub("std", "StandardDeviation", CNames)

CNames <- gsub("^f", "FrequencyDomain", CNames)
CNames <- gsub("^t", "TimeDomain", CNames)

# use new labels as column names
colnames(ActivityData) <- CNames


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

Tidy_mean <- ActivityData %>% 
    group_by(SubjectId, Activity) %>%
    summarise_each(funs(mean))

# output to file "tidy_mean.txt"
write.table(Tidy_mean, "tidy_mean.txt", row.names = FALSE, 
            quote = FALSE)




















