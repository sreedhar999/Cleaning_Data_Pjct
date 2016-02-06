##[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

##This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

##Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

## 1.Merges the training data and the test data sets to create one data set.

##Read training dataset into R
x_train<-read.table("./train/X_train.txt", header=FALSE) 
y_train<-read.table("./train/y_train.txt", header=FALSE)
s_train<-read.table("./train/subject_train.txt", header=FALSE)

##Read test dataset into R
x_test<-read.table("./test/X_test.txt", header=FALSE) 
y_test<-read.table("./test/y_test.txt", header=FALSE)
s_test<-read.table("./test/subject_test.txt", header=FALSE)

##Read features into R
features<-read.table("features.txt", header=FALSE) 

##Apply names to the datasets
colnames(x_train)<-features$V2
colnames(x_test)<-features$V2
colnames(y_train)<-"label"
colnames(y_test)<-"label"
colnames(s_train)<-"subject"
colnames(s_test)<-"subject"

##Column combind the training dataset
train<-cbind(s_train, y_train, x_train)
test<-cbind(s_test, y_test, x_test)

##remove columns with duplicated variable names
train <- train[, !duplicated(colnames(train))]
test <- test[, !duplicated(colnames(test))]

library(dplyr)
train<-mutate(train, datatype="train")
test<-mutate(test, datatype="test")

##row combind the training dataset

alldata<-rbind(train, test)

## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

meanstd_data<-select(alldata, datatype, subject, label, contains("mean"), contains("std"))

## 3.Uses descriptive activity names to name the activities in the data set

labels<-read.table("activity_labels.txt", header=FALSE) 

meanstd_data$label<-factor(meanstd_data$label, levels=c(1, 2, 3, 4, 5, 6), 
                           labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING",
                                    "LAYING"))

## 4.Appropriately labels the data set with descriptive variable names.
## make the following change to the variable names:
##tBody     time-Body
##tGravity	time-Gravity
##Mag	      Magnitude
##Gyro	    Gyroscope
##Acc	      Accelerometer
##fBody	    fastFourierTransform_Body
##Freq	    Frequency
##BodyBody	Body

Readable_Name<-gsub ("tBody", "time-Body", names(meanstd), ignore.case=FALSE)
Readable_Name<-gsub ("tGravity", "time-Gravity", nmeanstd, ignore.case=FALSE)
Readable_Name<-gsub ("Mag", "Magnitude", nmeanstd, ignore.case=FALSE)
Readable_Name<-gsub ("Gyro", "Gyroscope", nmeanstd, ignore.case=FALSE)
Readable_Name<-gsub ("Acc", "Accelerometer", nmeanstd, ignore.case=FALSE)
Readable_Name<-gsub ("fBody", "fastFourierTransform-Body", nmeanstd, ignore.case=FALSE)
Readable_Name<-gsub ("Freq", "Frequency", nmeanstd, ignore.case=FALSE)
Readable_Name<-gsub ("BodyBody", "Body", nmeanstd, ignore.case=FALSE)

colnames(meanstd)<-Readable_Name

## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each 
##   variable for each activity and each subject.


average_data<-meanstd_data %>%
        select(-datatype) %>%
        group_by(subject, label) %>%
        summarise_each(funs(mean))

write.table(avedata, file="average_data.txt", row.names=FALSE, col.names=TRUE)