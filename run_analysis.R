run_analysis <- function() {
#** 0: Loading data **********************************************************#
  trainingData <- read.table("UCI HAR Dataset/train/x_train.txt")
  testData <- read.table("UCI HAR Dataset/test/x_test.txt")  
  subjectTrainingData <- read.table("UCI HAR Dataset/train/subject_train.txt")
  subjectTestData <- read.table("UCI HAR Dataset/test/subject_test.txt")
  trainingLabels <- read.table("UCI HAR Dataset/train/y_train.txt")
  testLabels <- read.table("UCI HAR Dataset/test/y_test.txt") 
  
  activityLabelMap <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  dataFeatures <- read.table("UCI HAR Dataset/features.txt")
  
#** 1: Merge training & test data to create 1 dataset. ***********************#
  measurementData <- rbind(trainingData, testData)
  subjectData <- rbind(subjectTrainingData, subjectTestData)
  activityLabels <- rbind(trainingLabels, testLabels)

#** 2: Extracts only the measurements on the mean and standard deviation for *#
#***** each measurement.  ****************************************************#
  measuredVariableNames <- dataFeatures[2]  
  measuredVariableNames <- t(measuredVariableNames)
  relevantVariableIndices <- grep("-mean\\(\\)|-std\\(\\)", measuredVariableNames)
  
  measurementData <- measurementData[,relevantVariableIndices]

#** 3: Uses descriptive activity names to name the activities in the data set #
  activityLabelMap[2] <- sapply(activityLabelMap[2], function(x) { 
                                              tolower(gsub("_", " ", x)) 
                                            })
  activityNames <- activityLabelMap[activityLabels[,1],2]

#** 4: Appropriately labels the data set with descriptive variable names. ****#
  dataset <- cbind(subjectData, activityNames, measurementData)

  relevantVariableNames <- measuredVariableNames[relevantVariableIndices]
  cleanVariableNames <- lapply(relevantVariableNames, function(names) {
                                    names <- gsub("-", " ", names)
                                    names <- gsub("\\(\\)", "", names)
                                    names <- gsub("^t", "time ", names)
                                    names <- gsub("^f", "frequency ", names)
                                    names <- gsub("std", "standard deviation", names)
                                    tolower(names)
  })

  names(dataset) <- c("subject number", "activity name", cleanVariableNames)

#** 5: From the data set in step 4, creates a second, independent tidy data set
#      with the average of each variable for each activity and each subject.

  tidyData <- aggregate(dataset, by=list(activity=dataset$activity,
                                         subject=dataset$subject),
                                 mean)
  
  # Remove the original activity & subject columns (now unnecessary)
  tidyData <- tidyData[,-c(3:4)]

  # Finally, write the clean table to file.
  write.table(tidyData, "tidy_data_set.txt", row.name=FALSE)
  
  # Output the tidy dataset.
  tidyData
}