#############################################################
#  Desiree Wilson
#  August 23, 2015
#  Project 01: Getting and Cleaning Data
#  Goal: To merge the FitBit test and train datasets together
#        and to calculate the averages of mean() and std()
#        variables.
#############################################################

#loading libraries:
library(downloader)


#creating a 'data' file and downloading the data:
if(!file.exists("/data")){dir.create("/data")}
theurl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download(theurl, destfile="/data/thedata.zip", mode="wb")

#unzipping the zip file:
unzip(zipfile="/data/thedata.zip", exdir="/data")


#setting "data' file path as the working directory: 
thepath <- file.path("/data", "UCI HAR Dataset")

##reading in the feature file:
#reading in the feature file (feature file):
features <- read.table(file.path(thepath, "features.txt"), header=FALSE)


##reading in the activity_labels file:
#reading in the ctivity_labels file (activity_labels file):
actlabs <- read.table(file.path(thepath, "activity_labels.txt"), header=FALSE)


##reading in the test files:
#reading in the y_test file (activity file):
ytest <- read.table(file.path(thepath, "test", "y_test.txt"), header=FALSE)
newytest <- ytest

#giving descriptive names in the y_test file:
newytest[(which(as.character(ytest[,1]) == '1')),1] <- as.character(actlabs[1,2])
newytest[(which(as.character(ytest[,1]) == '2')),1] <- as.character(actlabs[2,2])
newytest[(which(as.character(ytest[,1]) == '3')),1] <- as.character(actlabs[3,2])
newytest[(which(as.character(ytest[,1]) == '4')),1] <- as.character(actlabs[4,2])
newytest[(which(as.character(ytest[,1]) == '5')),1] <- as.character(actlabs[5,2])
newytest[(which(as.character(ytest[,1]) == '6')),1] <- as.character(actlabs[6,2])


#reading in the X_test file (measurement file):
xtest <- read.table(file.path(thepath, "test", "X_test.txt"), header=FALSE)


#reading in the subject_test file (participant file):
subtest <- read.table(file.path(thepath, "test", "subject_test.txt"), header=FALSE)


##reading in the train files:
#reading in the y_train file (activity file):
ytrain <- read.table(file.path(thepath, "train", "y_train.txt"), header=FALSE)
newytrain <- ytrain


#giving descriptive names in the y_test file:
newytrain[(which(as.character(ytrain[,1]) == '1')),1] <- as.character(actlabs[1,2])
newytrain[(which(as.character(ytrain[,1]) == '2')),1] <- as.character(actlabs[2,2])
newytrain[(which(as.character(ytrain[,1]) == '3')),1] <- as.character(actlabs[3,2])
newytrain[(which(as.character(ytrain[,1]) == '4')),1] <- as.character(actlabs[4,2])
newytrain[(which(as.character(ytrain[,1]) == '5')),1] <- as.character(actlabs[5,2])
newytrain[(which(as.character(ytrain[,1]) == '6')),1] <- as.character(actlabs[6,2])


#reading in the X_train file (measurement file):
xtrain <- read.table(file.path(thepath, "train", "X_train.txt"), header=FALSE)


#reading in the subject_train file (participant file):
subtrain <- read.table(file.path(thepath, "train", "subject_train.txt"), header=FALSE)


#creating the test dataset:
testdata <- cbind(subtest[,1], newytest, xtest)
colnames(testdata) <- c('ParticipantID', 'Activity', as.character(features[,2]))


#creating the train dataset:
traindata <- cbind(subtrain[,1], newytrain, xtrain)
colnames(traindata) <- c('ParticipantID', 'Activity', as.character(features[,2]))


#merging the datasets together:
totaldata <- rbind(testdata, traindata)


#writing the data to a file:
write.table(totaldata, file='mergedata.txt', col.names=TRUE, row.names=FALSE, quote=FALSE)


#extracting columns that have either 'mean' or 'std' in the column name:
meanidx <- grep('mean', colnames(totaldata))
stdidx <- grep('std', colnames(totaldata))
meanstdidx <- sort(c(meanidx,stdidx))
meanstddata <- totaldata[,c(1,2,meanstdidx)]


#calculating the averages of the variables in 'meanstddata':
actnum <- rbind(ytest, ytrain)
newmeanstddata <- meanstddata
newmeanstddata$Activity <- actnum[,1]
aggredata <- aggregate(newmeanstddata, by=list(newmeanstddata$ParticipantID, newmeanstddata$Activity), FUN=mean)
aggredata$Activity[which(as.character(aggredata$Activity) == '1')] <- as.character(actlabs[1,2])
aggredata$Activity[which(as.character(aggredata$Activity) == '2')] <- as.character(actlabs[2,2])
aggredata$Activity[which(as.character(aggredata$Activity) == '3')] <- as.character(actlabs[3,2])
aggredata$Activity[which(as.character(aggredata$Activity) == '4')] <- as.character(actlabs[4,2])
aggredata$Activity[which(as.character(aggredata$Activity) == '5')] <- as.character(actlabs[5,2])
aggredata$Activity[which(as.character(aggredata$Activity) == '6')] <- as.character(actlabs[6,2])


#writing the data to a file:
write.table(aggredata, file='averagedata.txt', col.names=TRUE, row.names=FALSE, quote=FALSE)


