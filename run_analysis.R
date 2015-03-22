## This function is used to create a less cryptic name for the variable headers
prettyVariableName <- function(variableName)
{
  variableName <- gsub("-mean\\(\\)","Mean", variableName)  #-mean() to Mean
  variableName <- gsub("-std\\(\\)","StdDev", variableName) #-str() to StdDev
  variableName <- gsub("[()]","", variableName)             #Removing parenthesis
  variableName <- gsub("[-]","", variableName)              #Removing dashes 
  variableName <- gsub("^f","Frequency", variableName)      #Replacing names that start with f
  variableName <- gsub("^t","Time", variableName)           #Replacing names that start with t
  variableName <- gsub("BodyBody","Body", variableName)     #Replacing names that start with BodyBody
  
  variableName
}


## This is the main processing script used to create tidy dataset from the data
  library(plyr)

  ############################
  ## Train data
  ############################
  
  # First load the train data 
  trainSet <- read.table('train/X_train.txt', sep="")
  
  # Add the lables
  trainSetLabels <- read.table('train/y_train.txt', sep="")
  names(trainSetLabels)[1] <- 'ActivityId'
  trainSet <- cbind(trainSet, trainSetLabels )
  
  # Add the subjects
  trainSetSubjects <- read.table('train/subject_train.txt', sep="")
  names(trainSetSubjects)[1] <- 'Subject'
  trainSet <- cbind(trainSet, trainSetSubjects)
  
  ############################
  ## Test data
  ############################
  
  # First load the test data
  testSet <- read.table('test/X_test.txt', sep="")
  
  # Add the labels
  testSetLabels <- read.table('test/y_test.txt', sep="")
  names(testSetLabels)[1] <- 'ActivityId'
  testSet <- cbind(testSet, testSetLabels)
  
  # Add the subjects
  testSetSubjects <- read.table('test/subject_test.txt', sep="")
  names(testSetSubjects)[1] <- 'Subject'
  testSet <- cbind(testSet, testSetSubjects)
  
  
  # Join both sets into a single one
  completeSet <- rbind(testSet, trainSet)
  
  #############################
  ## Set the column names 
  #############################
  
  # Get the column names 
  colNamesFile <- read.table('features.txt', sep="")
  
  # Get a vector with only the column names
  columnNames <- as.character(colNamesFile[,2])
  
  #add the activityId column name
  columnNames <- c(columnNames, "ActivityId")
  columnNames <- c(columnNames, "Subject")
  
  # rename complete set
  names(completeSet) <- columnNames
  
  # Load the activity table
  activitySet <- read.table('activity_labels.txt', sep="", col.names=c("ActivityId", "Activity"))
  
  #Join the activity names to the data
  joinedSet <- merge(completeSet, activitySet, by="ActivityId")
  
  #Repopulate the column names because there's additional columns that were not in the features.txt file
  #If we don't do this, the column indexes are not going to match with the dataset and we have trouble :)
  columnNames <- names(joinedSet)
  
  #Get only the columns with mean and std in the name
  destinationColumnNames <- grep("-mean\\(\\)|-std\\(\\)", columnNames)
  shortSet <- joinedSet[destinationColumnNames]
  shortSet <- cbind(shortSet, Subject = joinedSet$Subject)
  shortSet <- cbind(shortSet, Activity = joinedSet$Activity)
  
  shortSetColumns <- names(shortSet)
  names(shortSet) <- lapply(shortSetColumns, prettyVariableName)
  
  #Summarize the data by Subject and Activity
  tidyDataSet <- ddply(shortSet, .(Subject, Activity), .fun = function(x) { colMeans(x[,1:66]) } )
  
  write.table(tidyDataSet, "TidyData.txt", row.name=FALSE) 

