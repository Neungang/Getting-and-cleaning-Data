if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("dplyr")) {
  install.packages("dplyr")
}

require("data.table")
require("dplyr")

# read the Supporting Metadata

# setwd("C:\Users\Family Asonganyi\Documents\UCI HAR Dataset")
featureNames   <- read.table("UCI HAR Dataset/features.txt",header=FALSE)
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE)

# Read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
xTrain       <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
yTrain       <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)

# Read test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
yTest       <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
xTest       <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# 1. Merge the training and the test sets to create one data set
# combine the respective data in training and test data sets 
# corresponding to subject, activity and features.
subject  <- rbind(subjectTrain, subjectTest)
activity <- rbind(yTrain, yTest)
features <- rbind(xTrain, xTest)

# Naming the columns
colnames(features) <- t(featureNames[2])

# Merge the data
colnames(activity) <- "Activity"
colnames(subject)  <- "Subject"
completeData       <- cbind(features,activity,subject)

# 2. Extract only the measurements on the mean 
# and standard deviation for each measurement. 

# Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# 3. Use descriptive activity names to name the activities in the data set

# Add activity and subject columns to the list of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

# create extractedData with the selected columns in requiredColumns
extractedData <- completeData[,requiredColumns]

# The activity field in extractedData is originally of numeric type. 
# We need to change its type to character so that it can accept activity names.
# The activity names are taken from metadata activityLabels.
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

# We need to factor the activity variable, once the activity names are updated.
extractedData$Activity <- as.factor(extractedData$Activity)

# 4. Appropriately labels the data set with descriptive variable names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject

# set Subject as a factor variable.
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# create tidyData as a data set with average for each activity and subject.
# Then, we order the enties in tidyData and write it into data file Tidy.txt that 
# contains the processed data.
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)