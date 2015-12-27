## run_analysis.R

## Getting and Cleaning Data
## Programming Assignment 1
## Author: Rick Hutchison
## Created: 12/24/2015
## 
## This is a function to create the tidy dataset for the class project.

genTidyDS <- function(){
  ## this function assumes the working directory contains the source
  ## data and will be the destination for the resulting tidy data file.
  
  library(data.table)

  ##################
  ## part 1 - create single large unsummarized data set
  ##################
  
  ## load labels
  actlab <- read.table("./data/activity_labels.txt")
  featlab <- read.table("./data/features.txt")
  
  ## stage the new mean & std column names
  featlab$NEWCOLNM <- gsub("[[:punct:]]", '', featlab$V2)
  featlab$NEWCOLNM <- gsub('mean', 'Mean', featlab$NEWCOLNM)
  featlab$NEWCOLNM <- gsub('std', 'Std', featlab$NEWCOLNM)

  ## correct duplicate columns now
  featlab[c(303:316),"NEWCOLNM"] <- with(featlab[c(303:316),], paste0("X",NEWCOLNM))
  featlab[c(317:330),"NEWCOLNM"] <- with(featlab[c(317:330),], paste0("Y",NEWCOLNM))
  featlab[c(331:344),"NEWCOLNM"] <- with(featlab[c(331:344),], paste0("Z",NEWCOLNM))
  featlab[c(382:395),"NEWCOLNM"] <- with(featlab[c(382:395),], paste0("X",NEWCOLNM))
  featlab[c(396:409),"NEWCOLNM"] <- with(featlab[c(396:409),], paste0("Y",NEWCOLNM))
  featlab[c(410:423),"NEWCOLNM"] <- with(featlab[c(410:423),], paste0("Z",NEWCOLNM))
  featlab[c(461:474),"NEWCOLNM"] <- with(featlab[c(461:474),], paste0("X",NEWCOLNM))
  featlab[c(475:488),"NEWCOLNM"] <- with(featlab[c(475:488),], paste0("Y",NEWCOLNM))
  featlab[c(489:502),"NEWCOLNM"] <- with(featlab[c(489:502),], paste0("Z",NEWCOLNM))
  
  ## process new activity labels
  newactlab <- data.frame(NEWACTNM = c("Walking","WalkingUpstairs","WalkingDownstairs","Sitting","Standing","Laying"))
  actlab <- cbind(actlab,newactlab)
  
  ## process subject data
  ## single column corresponds to row of txtx/trnx
  tstsubj <- read.table("./data/subject_test.txt")
  trnsubj <- read.table("./data/subject_train.txt")
  colnames(tstsubj) <- c("Subject")
  colnames(trnsubj) <- c("Subject")
  
  ## process activity data
  ## single column corresponds to row of txtx/trnx, contains value associated with actlab
  ## change value to text string based on activity labels (actlab)
  tsty <- read.table("./data/y_test.txt")
  trny <- read.table("./data/y_train.txt")
  colnames(tsty) <- c("Activity")
  colnames(trny) <- c("Activity")
  
  ## columns correspond to features entries
  tstx <- read.table("./data/X_test.txt")
  trnx <- read.table("./data/X_train.txt")
  colnames(tstx) <- featlab$NEWCOLNM
  colnames(trnx) <- featlab$NEWCOLNM
  
  ## merge into large dataset
  lgtst <- cbind(tstsubj,tsty,tstx)
  lgtrn <- cbind(trnsubj,trny,trnx)
  largeds <- rbind(lgtst,lgtrn)
  
  ## replace activity code with description, purge unneeded columns
  largeds <- data.table(largeds)
  actlab <- data.table(actlab)
  largeds <- merge(actlab, largeds, by.x="V1", by.y="Activity", all=TRUE)
  largeds <- largeds[, c(4,3,5:565), with = FALSE]
  names(largeds)[names(largeds) == 'NEWACTNM'] <- 'Activity'

  ##################
  ## part 2 - create summarized tidy data set
  ##################
  
  ## summarize, averaging data values
  tidyds <- largeds[,lapply(.SD,mean),by=c("Subject","Activity"),.SDcols=3:563]
  
  ## filter feat for records with only mean or std (but no meanFreq or stdFreq) in column V2
  feat <- data.table(featlab)
  subfeat <- feat[intersect(grep("mean|std",V2),grep("Freq",V2,invert=TRUE))]
  
  ## subset the summarized large dataset
  tidyds <- data.frame(tidyds)
  tidyds <- tidyds[c("Subject","Activity",subfeat$NEWCOLNM)]
  
  write.table(tidyds,"./tidyds.txt", row.names = FALSE)
}