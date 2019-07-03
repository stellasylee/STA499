library(stringr)
library(tidyverse)
library ("readxl")
library ("dplyr")
library (changepoint)

disp <- read.csv("H:\\NIDA\\dispositionUpdate.csv")
fileNames <- disp$DaqName

# paramaters: 
# s: start point for detecting secondary task
# valid: for first task to filter non-zero value at the start
# return:
# start: rowname of task available
# end: when task was completed/evaluated as correct
detectTime <- function (file, s, valid){
  # start = when task was available
  for (start in s: nrow(file)){
    if ((valid == TRUE) && (file$SCC.MenuSearch[start] != 0)){
      break
    }else if (file$SCC.MenuSearch[start] == 0){
      valid <- TRUE
    }
  }

  for (end in (start+1): nrow(file)){
    if (file$AUX1.MenuScore[end] > 0){
      break
    }
  }
  return (c(start, end))
}

# Create dataframe for analysis (with available time as starting point of experiment group)
analysisMatrix <- data.frame(matrix(ncol = 8, nrow = 0))
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  # For every artist task in the simulation
  for (eventNum in 1:3){
    start <- 1
    valid <- FALSE
    if (eventNum != 1){
      start <- end +1
      valid <- TRUE
    }
    times <- detectTime(file, start, valid)
    start <- times[1] # when the task is available
    end <- times[2] # when the participant entered correct answer
    # get output variables for experiment group
    exp <- 1
    sdLane <- sd(file$SCC.Lane.Deviation.2[start:end])
    avgSpeed <- mean(file$VDS.Veh.Speed[start:end])
    sdSpeed <- sd(file$VDS.Veh.Speed[start:end])
    analysisMatrix <- rbind(analysisMatrix, 
                            c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                              exp, eventNum, sdLane, avgSpeed, sdSpeed),
                            stringsAsFactors = FALSE)
    # get output variables for control group
    exp <- 0
    sdLane <- sd(file$SCC.Lane.Deviation.2[(start-(end-start)):start])
    avgSpeed <- mean(file$VDS.Veh.Speed[(start-(end-start)):start])
    sdSpeed <- sd(file$VDS.Veh.Speed[(start-(end-start)):start])
    analysisMatrix <- rbind(analysisMatrix, 
                            c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                              exp, eventNum, sdLane, avgSpeed, sdSpeed),
                            stringsAsFactors = FALSE)
  }
}
colnames(analysisMatrix) <- c("ID", "DaqName", "DosingLevel", "Experiment", "eventNum",
                              "SD.Lane.Deviation", "Avg.Speed", "SD.Speed")
analysisMatrix$ID <- as.numeric(analysisMatrix$ID)
analysisMatrix$Experiment <- as.numeric(analysisMatrix$Experiment)
analysisMatrix$SD.Lane.Deviation <- as.numeric(analysisMatrix$SD.Lane.Deviation)
analysisMatrix$Avg.Speed <- as.numeric(analysisMatrix$Avg.Speed)
analysisMatrix$SD.Speed <- as.numeric(analysisMatrix$SD.Speed)
write.csv(analysisMatrix, file = "H:\\NIDA\\analysisWithoutEngagementPoint.csv", row.names=FALSE)
