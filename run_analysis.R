#Read feature and activity table
featureNames <- read.table("./UCI HAR Dataset/features.txt")
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Read test data sets
xtest<-read.table("./UCI HAR Dataset/test/X_test.txt")
ytest<-read.table("./UCI HAR Dataset/test/y_test.txt")
subjecttest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
#Read train data sets
xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
subjecttrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")

#Merge test and train data sets
feature<-merge(xtest,xtrain,all=TRUE)
activity<-rbind(ytest,ytrain)
subject<-merge(subjecttest,subjecttrain,all=TRUE)

#Add label to data sets
colnames(feature)<-featureNames[,2]
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

#Extracts only the measurements on the mean and standard deviation for each measurement
meansdcol<-grep(".*Mean.*|.*Std.*", names(feature), ignore.case=TRUE)
extractfeat<-feature[,meansdcol]
complete<-cbind(extractfeat,activity,subject)

#Change descriptive activity names to name the activities in the data set
complete$Activity <- as.character(complete$Activity)
for (i in 1:6){
  complete$Activity[complete$Activity == i] <- as.character(activityLabels[i,2])
}
complete$Activity <- as.factor(complete$Activity)

#Appropriately labels the data set with descriptive variable names
names(complete)<-gsub("Acc", "Accelerometer", names(complete))
names(complete)<-gsub("Gyro", "Gyroscope", names(complete))
names(complete)<-gsub("BodyBody", "Body", names(complete))
names(complete)<-gsub("Mag", "Magnitude", names(complete))
names(complete)<-gsub("^t", "Time", names(complete))
names(complete)<-gsub("^f", "Frequency", names(complete))
names(complete)<-gsub("tBody", "TimeBody", names(complete))
names(complete)<-gsub("-mean()", "Mean", names(complete), ignore.case = TRUE)
names(complete)<-gsub("-std()", "STD", names(complete), ignore.case = TRUE)
names(complete)<-gsub("-freq()", "Frequency", names(complete), ignore.case = TRUE)
names(complete)<-gsub("angle", "Angle", names(complete))
names(complete)<-gsub("gravity", "Gravity", names(complete))

#create independent tidy data set with the average of each variable for each activity and each subject.
#load data.table library
library(data.table)
complete$Subject <- as.factor(complete$Subject)
complete <- data.table(complete)
#create tidyData as a data set with average for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, complete, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
