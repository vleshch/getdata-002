## Step 0, help function
readRawData <- function(dataCode) {
  filePath <- file.path(dataCode, paste0("y_", dataCode, ".txt"))
  yData <- read.table(filePath, header=F, col.names=c("ActivityID"))
  
  filePath <- file.path(dataCode, paste0("subject_", dataCode, ".txt"))
  subjects <- read.table(filePath, header=F, col.names=c("SubjectID"))
  

  colNames <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  filePath <- file.path(dataCode, paste0("X_", dataCode, ".txt"))
  rawData <- read.table(filePath, header=F, col.names=colNames$MeasureName)
  
  subCols <- grep(".*mean\\(\\)|.*std\\(\\)", colNames$MeasureName)
  
  rawData <- rawData[,subCols]
  
  rawData$ActivityID <- yData$ActivityID
  rawData$SubjectID <- subjects$SubjectID
  
  # return result
  rawData
}



#main function
run_analysis <- function() {

  #merge
  test=readData("test")
  train=readData("train")
  mergedData <- rbind(test, train)
  
  #apply labels
  activityLabels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
  activityLabels$ActivityName <- as.factor(activityLabels$ActivityName)
  dataLabeled <- merge(mergedData , activityLabels)
  
  
  library(reshape2)
  ids = c("ActivityID", "ActivityName", "SubjectID")
  measured = setdiff(colnames(dataLabeled), ids)
  meltedData <- melt(dataLabeled, id=ids, measure.vars=measured)
  
  # recast
  tidyData<-dcast(meltedData, ActivityName + SubjectID ~ variable, mean)
  
  # Rename
  colNames <- colnames(tidyData)
  colNames <- gsub("\\.+mean\\.+", colNames, replacement="Mean")
  colNames <- gsub("\\.+std\\.+", colNames, replacement="Std")
  colnames(tidyData) <- colNames
  
  write.table(tidyData, "tidy.txt")
}

run_analysis()
