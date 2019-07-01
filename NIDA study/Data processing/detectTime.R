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

# Get matrix for every simulation
for (i in 1:length(fileNames)){
  # x-coordinates ----
  
  # y-coordinates ----
  # detect times for secondary task ----
  times <- detectTime(fileNames[i])
  start <- times[1]
  end <- times[2]
  engage <- start + detectEngagement(file, start, end)
  
  # get output variables
}
