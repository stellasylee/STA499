library(stringr)
library(tidyverse)
library ("readxl")
library ("dplyr")
library (changepoint)

disp <- read.csv("H:\\NIDA\\dispositionUpdate.csv")
fileNames <- disp$DaqName

#' detectTime
#' finds when artist/menu task was available (start) and completed (end) searching from certain point
#' @param s start point for detecting secondary task
#' @param valid for first task to filter non-zero value at the start
#' @param eventNum instance number in the simulation helping which is valid MenuSearch change
#' @return c(start, end)
#' start: rowname of task available
#' end: when task was completed/evaluated as correct
detectTime <- function (file, s, valid, eventNum){
  correctResponse <- switch(eventNum,
                            c(1, 4, 7, 10, 16, 19),
                            c(2, 5, 8, 11, 17, 20),
                            c(3, 6, 9, 12, 18, 21))
  # start = when task was available
  for (start in s: nrow(file)){
    if ((valid == TRUE) && (file$SCC.MenuSearch[start] %in% correctResponse)){
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

#' detectEngagement
#' detect participant's engagement time during timeframe based on changepoint AMOC method
#' this functions determines engagement time by minimum of changepoint by speed and lane deviation
#' @param file simulation csv file converted to dataframe
#' @param start task available time
#' @param end task completed time
#' precondition (end - start) > 4 in order to detect changepoint
#' @return 
#' engage predicted engagement point during the task
#' FALSE if the predicted engagement point is invalid (predicted after the participant pressed incorrect answer)
detectEngagement <- function (file, start, end){
  speed <- file$VDS.Veh.Speed[start:end]
  s <- cpts(cpt.meanvar(speed, method = 'AMOC'))
  lane <- file$SCC.Lane.Deviation.2[start:end]
  l <- cpts(cpt.meanvar(lane, method = 'AMOC'))
  # our predicted engagement point is minimum between changepoint detection point in speed and lane deviation by mean and variance
  engage <- min(c(s,l))
  # Check this detected engagement point is before 'incorrect' answer if there was
  for (incorrect in (start+1): end){
    if (file$AUX1.MenuScore[incorrect] == -1){
      break
    }
  }
  # return valid engagement point
  if (engage < incorrect){
    return (engage) 
  }else return (FALSE)
}

# Shows start, engagement point, end and total available time for file
#' checkTrend
#' @param filename filename in designated directory
#' @return statement contains information about total task completion time, engagement point, start and end time
#' or contains filename and instance number when engagement point is invaild
checkTrend <- function (filename) {
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", filename))
  statement <- ""
  events <- 1:3
  if (fileNames[i] %in% c("20140111102408.csv", "20140104090136.csv")){
    events <- c(1, 3)
  }
  for (eventNum in events){
    start <- 1
    valid <- FALSE
    if (eventNum != 1){
      start <- end +1
      valid <- TRUE
    }
    times <- detectTime(file, start, valid, eventNum)
    start <- times[1] # when the task is available
    end <- times[2] # when the participant entered correct answer
    print(paste0(start, " ", end))
    engage <- start + detectEngagement(file, start, end)
    if (engage != FALSE){
      statement <- (paste0(statement, "total: " , (end - start), 
                           " start: ", start, " engage: ", engage, " end: ", end, "\n"))
    }else statement <- paste0(statement, eventNum, filename, "\n")
  }
  return (statement)
}

# Checks if there is any simulation data with invalid engagement point
for (i in 1:length(fileNames)){
  print (paste0("participantID: ",disp$ID[i], " ", checkTrend(fileNames[i])))
}

# Save secondary task time detection data to csv for future analysis ----
## Create dataframe to store information
eventTimes <- data.frame(matrix(ncol = 8, nrow = 0))
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  events <- 1:3
  if (fileNames[i] %in% c("20140111102408.csv", "20140104090136.csv")){
    events <- c(1, 3)
  }
  for (eventNum in events){
    start <- 1
    valid <- FALSE
    if (eventNum != 1){
      start <- end +1
      valid <- TRUE
    }
    times <- detectTime(file, start, valid, eventNum)
    start <- times[1] # when the task is available
    end <- times[2] # when the participant entered correct answer
    engage <- start + detectEngagement(file, start, end)
    eventTimes <- rbind.data.frame(eventTimes, 
                                   c(disp$ID[i], as.character(fileNames[i]), as.character(disp$DosingLevel[i]),
                                     eventNum, start, engage, end, (end - start)),
                                   stringsAsFactors = FALSE)
  }
}
colnames(eventTimes) <- c("ID", "DaqName", "DosingLevel", "eventNum", "start", "engagement", "end", "total")
eventTimes$ID <- as.numeric(eventTimes$ID)
eventTimes$eventNum <- as.numeric(eventTimes$eventNum)
eventTimes$start <- as.numeric(eventTimes$start)
eventTimes$engagement <- as.numeric(eventTimes$engagement)
eventTimes$end <- as.numeric(eventTimes$end)
eventTimes$total <- as.numeric(eventTimes$total)
eventTimes <- eventTimes[order(eventTimes$ID, eventTimes$DosingLevel),]
write.csv(eventTimes, file = "H:\\NIDA\\eventTimesWith3.csv", row.names=FALSE)

# Create dataframe for analysis (with predicted engagement point as start of experiment group) ----
analysisMatrix <- data.frame(matrix(ncol = 8, nrow = 0))
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  # For every artist task in the simulation
  events <- 1:3
  if (fileNames[i] %in% c("20140111102408.csv", "20140104090136.csv")){ 
    events <- c(1, 3) # these simulation doesn't have second instance engagement by participant
  }
  for (eventNum in events){
    start <- 1
    valid <- FALSE
    if (eventNum != 1){
      start <- end +1
      valid <- TRUE
    }
    times <- detectTime(file, start, valid, eventNum)
    start <- times[1] # when the task is available
    end <- times[2] # when the participant entered correct answer
    engage <- start + detectEngagement(file, start, end)
    # get output variables for experiment group
    exp <- 1
    sdLane <- sd(file$SCC.Lane.Deviation.2[engage:end])
    avgSpeed <- mean(file$VDS.Veh.Speed[engage:end])
    sdSpeed <- sd(file$VDS.Veh.Speed[engage:end])
    analysisMatrix <- rbind(analysisMatrix, 
                            c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                              exp, eventNum, sdLane, avgSpeed, sdSpeed),
                            stringsAsFactors = FALSE)
    # get output variables for control group
    exp <- 0
    sdLane <- sd(file$SCC.Lane.Deviation.2[(start-(end-engage)):start])
    avgSpeed <- mean(file$VDS.Veh.Speed[(start-(end-engage)):start])
    sdSpeed <- sd(file$VDS.Veh.Speed[(start-(end-engage)):start])
    analysisMatrix <- rbind(analysisMatrix, 
                            c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                              exp, eventNum, sdLane, avgSpeed, sdSpeed),
                            stringsAsFactors = FALSE)
  }
}
colnames(analysisMatrix) <- c("ID", "DaqName", "DosingLevel", "Experiment", "eventNum",
                              "SD.Lane.Deviation", "Avg.Speed", "SD.Speed")
analysisMatrix$ID <- as.numeric(analysisMatrix$ID)
analysisMatrix$eventNum <- as.numeric(analysisMatrix$eventNum)
analysisMatrix$Experiment <- as.numeric(analysisMatrix$Experiment)
analysisMatrix$SD.Lane.Deviation <- as.numeric(analysisMatrix$SD.Lane.Deviation)
analysisMatrix$Avg.Speed <- as.numeric(analysisMatrix$Avg.Speed)
analysisMatrix$SD.Speed <- as.numeric(analysisMatrix$SD.Speed)
write.csv(analysisMatrix, file = "H:\\NIDA\\analysisTasks3.csv", row.names=FALSE)

# Create dataframe for analysis (with task available time as start of experiment group) ----
analysisMatrix <- data.frame(matrix(ncol = 8, nrow = 0))
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  # For every artist task in the simulation
  events <- 1:3
  if (fileNames[i] %in% c("20140111102408.csv", "20140104090136.csv")){ 
    events <- c(1, 3) # these simulation doesn't have second instance engagement by participant
  }
  for (eventNum in events){
    start <- 1
    valid <- FALSE
    if (eventNum != 1){
      start <- end +1
      valid <- TRUE
    }
    times <- detectTime(file, start, valid, eventNum)
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
analysisMatrix$eventNum <- as.numeric(analysisMatrix$eventNum)
analysisMatrix$Experiment <- as.numeric(analysisMatrix$Experiment)
analysisMatrix$SD.Lane.Deviation <- as.numeric(analysisMatrix$SD.Lane.Deviation)
analysisMatrix$Avg.Speed <- as.numeric(analysisMatrix$Avg.Speed)
analysisMatrix$SD.Speed <- as.numeric(analysisMatrix$SD.Speed)
write.csv(analysisMatrix, file = "H:\\NIDA\\analysisTasksNoEngage3.csv", row.names=FALSE)
