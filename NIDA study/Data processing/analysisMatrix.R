library(stringr)
library(tidyverse)
library ("readxl")
library ("dplyr")
library (changepoint)

disp <- read.csv("H:\\NIDA\\dispositionUpdate.csv")
fileNames <- disp$DaqName

file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[1]))

# return:
# start: rowname of task available
# end: when task was completed/evaluated as correct
detectTime <- function (file){
  valid <- FALSE # Some simulation had non-zero value at the start
  # start = when task was available
  for (start in 1: nrow(file)){
    if ((valid == TRUE) && (file$SCC.MenuSearch[start] != 0)){
      break
    }else if (file$SCC.MenuSearch[start] == 0){
      valid <- TRUE
    }
  }
  valid <- FALSE
  # end = when task was completed
  for (end in (start+1): nrow(file)){
    if ((valid == TRUE) && (file$AUX1.MenuScore[end] > 0)){
      break
    }else if (file$AUX1.MenuScore[end] == 0){
      valid <- TRUE
    }
  }
  return (c(start, end))
}
times<- detectTime(file)

# return
# engage: predicted engagement point during the task
# FALSE: if the predicted engagement point is invalid, which means it was predicted after the participant pressed incorrect answer
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
checkTrend <- function (filename) {
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", filename))
  times <- detectTime (file)
  start <- times[1]
  end <- times[2]
  engage <- detectEngagement(file, start, end)
  if (engage != FALSE){
    return (paste0("total: " , (end - start), " engage: ", engage, 
                   " start: ", start," engagement:", (start+ engage), " end: ", end))
  }else return (filename)
}

# Checks if there is any simulation data with invalid engagement point
for (i in 1:length(fileNames)){
  print (paste0("participantID: ",disp$ID[i], " ", checkTrend2(fileNames[i])))
}

# Save secondary task time detection data to csv for future analysis
eventTimes <- data.frame(matrix(ncol = 7, nrow = 0))
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  times <- detectTime (file)
  start <- times[1]
  end <- times[2]
  engage <- detectEngagement(file, start, end)
  eventTimes <- rbind.data.frame(eventTimes, 
                                 c(disp$ID[i], as.character(fileNames[i]), as.character(disp$DosingLevel[i]),
                                   start, (start + engage), end, (end - start)),
                                 stringsAsFactors = FALSE)
}
colnames(eventTimes) <- c("ID", "DaqName", "DosingLevel", "start", "engagement", "end", "total")
eventTimes$ID <- as.numeric(eventTimes$ID)
eventTimes$start <- as.numeric(eventTimes$start)
eventTimes$engagement <- as.numeric(eventTimes$engagement)
eventTimes$end <- as.numeric(eventTimes$end)
eventTimes$total <- as.numeric(eventTimes$total)
eventTimes <- eventTimes[order(eventTimes$ID, eventTimes$DosingLevel),]
write.csv(eventTimes, file = "H:\\NIDA\\eventTimes.csv", row.names=FALSE)

# Create dataframe for analysis
analysisMatrix <- data.frame(matrix(ncol = 7, nrow = 0))
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  # detect times for secondary task ----
  times <- detectTime(file)
  start <- times[1]
  end <- times[2]
  engage <- start + detectEngagement(file, start, end)
  
  # get output variables for experiment group
  exp <- 1
  sdLane <- sd(file$SCC.Lane.Deviation.2[engage:end])
  avgSpeed <- mean(file$VDS.Veh.Speed[engage:end])
  sdSpeed <- sd(file$VDS.Veh.Speed[engage:end])
  analysisMatrix <- rbind(analysisMatrix, 
                          c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                            exp, sdLane, avgSpeed, sdSpeed),
                          stringsAsFactors = FALSE)
  # get output variables for control group
  exp <- 0
  sdLane <- sd(file$SCC.Lane.Deviation.2[(start-(end-engage)):start])
  avgSpeed <- mean(file$VDS.Veh.Speed[(start-(end-engage)):start])
  sdSpeed <- sd(file$VDS.Veh.Speed[(start-(end-engage)):start])
  analysisMatrix <- rbind(analysisMatrix, 
                          c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                            exp, sdLane, avgSpeed, sdSpeed),
                          stringsAsFactors = FALSE)
}
colnames(analysisMatrix) <- c("ID", "DaqName", "DosingLevel", "Experiment",
                              "SD.Lane.Deviation", "Avg.Speed", "SD.Speed")
analysisMatrix$ID <- as.numeric(analysisMatrix$ID)
analysisMatrix$Experiment <- as.numeric(analysisMatrix$Experiment)
analysisMatrix$SD.Lane.Deviation <- as.numeric(analysisMatrix$SD.Lane.Deviation)
analysisMatrix$Avg.Speed <- as.numeric(analysisMatrix$Avg.Speed)
analysisMatrix$SD.Speed <- as.numeric(analysisMatrix$SD.Speed)
write.csv(analysisMatrix, file = "H:\\NIDA\\analysisMatrix.csv", row.names=FALSE)
