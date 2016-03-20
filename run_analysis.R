library(dplyr)
library(data.table)
library(tidyr)


filep<-"C:/Users/turla/OneDrive/Documents/Getting and Cleaning data/course project/UCI HAR Dataset"
# Reading files
data_SubjectTrain <- tbl_df(read.table(file.path(filep, "train", "subject_train.txt")))
data_SubjectTest  <- tbl_df(read.table(file.path(filep, "test" , "subject_test.txt" )))
data_activitytrain <- tbl_df(read.table(file.path(filep, "train", "Y_train.txt")))
data_activitytest  <- tbl_df(read.table(file.path(filep, "test" , "Y_test.txt" )))
data_Train <- tbl_df(read.table(file.path(filep, "train", "X_train.txt" )))
data_Test  <- tbl_df(read.table(file.path(filep, "test" , "X_test.txt" )))
# Merging Activity and Subject files and reb=naming the variables 
data_Subject <- rbind(data_SubjectTrain, data_SubjectTest)
setnames(data_Subject, "V1", "subject")
data_Activity<- rbind(data_activitytrain, data_activitytest)
setnames(data_Activity, "V1", "activityNum")

#Merging the data training and test files into one data table
datatable <- rbind(data_Train, data_Test)

# naming variables according to feature 
dataFeatures <- tbl_df(read.table(file.path(filep, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNumber", "featureName"))
colnames(datatable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filep, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNumber","activityName"))

# Join columns
dataSubject<- cbind(data_Subject, data_Activity)
dataset <- cbind(dataSubject, datatable)

# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) 

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNumber"), dataFeaturesMeanStd)
dataset<- subset(dataset,select=dataFeaturesMeanStd) 

##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNumber", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

###names
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
head(str(dataTable),6)

##Putting all data in a new table
write.table(dataTable, "tidy_Data.csv", row.name=FALSE)
new_datatable<-read.csv("tidy_Data.csv")


