require(dplyr)
train<-read.table(".//train//X_train.txt",sep="",header=F,na.strings="",stringsAsFactors = F)
test<-read.table(".//test//X_test.txt",sep="",header=F,na.strings="",stringsAsFactors = F)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
features<-read.csv("features.txt",sep=" ",header=FALSE)
colnames(features)<-c("id","measure")
idx<-grep("mean\\(\\)|std\\(\\)",features$measure)
features[idx,]

test<-test[,idx]
train<-train[,idx]

#4.Appropriately labels the data set with descriptive variable names.
names(train)<-features[idx,2]
names(test)<-features[idx,2]

#get labels
activity<-read.csv("activity_labels.txt",sep="",header=F)
names(activity)<-c("id","activity_label")
#for test data
ytest<-read.csv(".//test//y_test.txt",header=F)
names(ytest)<-c("id")
index<-match(ytest$id,activity$id)
subject<-read.csv(".//test//subject_test.txt",header=F)
test["subject"]<-subject[1]
#3.Uses descriptive activity names to name the activities in the data set
test["activity_label"]<-activity[index,2]

#for train data
ytrain<-read.csv(".//train//y_train.txt",header=F)
names(ytrain)<-c("id")
index<-match(ytrain$id,activity$id)
subject<-read.csv(".//train//subject_train.txt",header=F)
train["subject"]<-subject[1]
#3.Uses descriptive activity names to name the activities in the data set
train["activity_label"]<-activity[index,2]


#1.Merges the training and the test sets to create one data set.
total<-rbind(train,test)

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
act<-group_by(total,subject,activity_label)
subset<-summarize(act
          ,`tBodyAcc-mean()-X`		  =mean(`tBodyAcc-mean()-X`		  )
          ,`tBodyAcc-mean()-Y`          =mean(`tBodyAcc-mean()-Y`          )
          ,`tBodyAcc-mean()-Z`          =mean(`tBodyAcc-mean()-Z`          )
          ,`tBodyAcc-std()-X`           =mean(`tBodyAcc-std()-X`           )
          ,`tBodyAcc-std()-Y`           =mean(`tBodyAcc-std()-Y`           )
          ,`tBodyAcc-std()-Z`           =mean(`tBodyAcc-std()-Z`           )
          ,`tGravityAcc-mean()-X`       =mean(`tGravityAcc-mean()-X`       )
          ,`tGravityAcc-mean()-Y`       =mean(`tGravityAcc-mean()-Y`       )
          ,`tGravityAcc-mean()-Z`       =mean(`tGravityAcc-mean()-Z`       )
          ,`tGravityAcc-std()-X`        =mean(`tGravityAcc-std()-X`        )
          ,`tGravityAcc-std()-Y`        =mean(`tGravityAcc-std()-Y`        )
          ,`tGravityAcc-std()-Z`        =mean(`tGravityAcc-std()-Z`        )
          ,`tBodyAccJerk-mean()-X` 	  =mean(`tBodyAccJerk-mean()-X` 	  )
          ,`tBodyAccJerk-mean()-Y` 	  =mean(`tBodyAccJerk-mean()-Y` 	  )
          ,`tBodyAccJerk-mean()-Z`      =mean(`tBodyAccJerk-mean()-Z`      )
          ,`tBodyAccJerk-std()-X`       =mean(`tBodyAccJerk-std()-X`       )
          ,`tBodyAccJerk-std()-Y`       =mean(`tBodyAccJerk-std()-Y`       )
          ,`tBodyAccJerk-std()-Z`       =mean(`tBodyAccJerk-std()-Z`       )
          ,`tBodyGyro-mean()-X`         =mean(`tBodyGyro-mean()-X`         )
          ,`tBodyGyro-mean()-Y`         =mean(`tBodyGyro-mean()-Y`         )
          ,`tBodyGyro-mean()-Z`         =mean(`tBodyGyro-mean()-Z`         )
          ,`tBodyGyro-std()-X`          =mean(`tBodyGyro-std()-X`          )
          ,`tBodyGyro-std()-Y`          =mean(`tBodyGyro-std()-Y`          )
          ,`tBodyGyro-std()-Z`          =mean(`tBodyGyro-std()-Z`          )
          ,`tBodyGyroJerk-mean()-X`     =mean(`tBodyGyroJerk-mean()-X`     )
          ,`tBodyGyroJerk-mean()-Y`     =mean(`tBodyGyroJerk-mean()-Y`     )
          ,`tBodyGyroJerk-mean()-Z`     =mean(`tBodyGyroJerk-mean()-Z`     )
          ,`tBodyGyroJerk-std()-X`      =mean(`tBodyGyroJerk-std()-X`      )
          ,`tBodyGyroJerk-std()-Y`      =mean(`tBodyGyroJerk-std()-Y`      )
          ,`tBodyGyroJerk-std()-Z`      =mean(`tBodyGyroJerk-std()-Z`      )
          ,`tBodyAccMag-mean()`         =mean(`tBodyAccMag-mean()`         )
          ,`tBodyAccMag-std()`          =mean(`tBodyAccMag-std()`          )
          ,`tGravityAccMag-mean()`      =mean(`tGravityAccMag-mean()`      )
          ,`tGravityAccMag-std()`       =mean(`tGravityAccMag-std()`       )
          ,`tBodyAccJerkMag-mean()`     =mean(`tBodyAccJerkMag-mean()`     )
          ,`tBodyAccJerkMag-std()`      =mean(`tBodyAccJerkMag-std()`      )
          ,`tBodyGyroMag-mean()`        =mean(`tBodyGyroMag-mean()`        )
          ,`tBodyGyroMag-std()`         =mean(`tBodyGyroMag-std()`         )
          ,`tBodyGyroJerkMag-mean()`    =mean(`tBodyGyroJerkMag-mean()`    )
          ,`tBodyGyroJerkMag-std()`     =mean(`tBodyGyroJerkMag-std()`     )
          ,`fBodyAcc-mean()-X`          =mean(`fBodyAcc-mean()-X`          )
          ,`fBodyAcc-mean()-Y`          =mean(`fBodyAcc-mean()-Y`          )
          ,`fBodyAcc-mean()-Z`          =mean(`fBodyAcc-mean()-Z`          )
          ,`fBodyAcc-std()-X`           =mean(`fBodyAcc-std()-X`           )
          ,`fBodyAcc-std()-Y`           =mean(`fBodyAcc-std()-Y`           )
          ,`fBodyAcc-std()-Z`           =mean(`fBodyAcc-std()-Z`           )
          ,`fBodyAccJerk-mean()-X`      =mean(`fBodyAccJerk-mean()-X`      )
          ,`fBodyAccJerk-mean()-Y`      =mean(`fBodyAccJerk-mean()-Y`      )
          ,`fBodyAccJerk-mean()-Z`      =mean(`fBodyAccJerk-mean()-Z`      )
          ,`fBodyAccJerk-std()-X`       =mean(`fBodyAccJerk-std()-X`       )
          ,`fBodyAccJerk-std()-Y`       =mean(`fBodyAccJerk-std()-Y`       )
          ,`fBodyAccJerk-std()-Z`       =mean(`fBodyAccJerk-std()-Z`       )
          ,`fBodyGyro-mean()-X`         =mean(`fBodyGyro-mean()-X`         )
          ,`fBodyGyro-mean()-Y`         =mean(`fBodyGyro-mean()-Y`         )
          ,`fBodyGyro-mean()-Z`         =mean(`fBodyGyro-mean()-Z`         )
          ,`fBodyGyro-std()-X`          =mean(`fBodyGyro-std()-X`          )
          ,`fBodyGyro-std()-Y`          =mean(`fBodyGyro-std()-Y`          )
          ,`fBodyGyro-std()-Z`          =mean(`fBodyGyro-std()-Z`          )
          ,`fBodyAccMag-mean()`         =mean(`fBodyAccMag-mean()`         )
          ,`fBodyAccMag-std()`          =mean(`fBodyAccMag-std()`          )
          ,`fBodyBodyAccJerkMag-mean()` =mean(`fBodyBodyAccJerkMag-mean()` )
          ,`fBodyBodyAccJerkMag-std()`  =mean(`fBodyBodyAccJerkMag-std()`  )
          ,`fBodyBodyGyroMag-mean()`    =mean(`fBodyBodyGyroMag-mean()`    )
          ,`fBodyBodyGyroMag-std()`     =mean(`fBodyBodyGyroMag-std()`     )
          ,`fBodyBodyGyroJerkMag-mean()`=mean(`fBodyBodyGyroJerkMag-mean()`) 
          ,`fBodyBodyGyroJerkMag-std()` =mean(`fBodyBodyGyroJerkMag-std()` ))
write.table(subset,file="cleansubset.txt",row.name=FALSE)

