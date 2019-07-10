library(changepoint)
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(tidyverse)

#writing function to detect the start and event of side mirror task
mirrorDetect <- function (file, s){
  for (start in s:nrow(file)) {
    if ((file$SCC.MirrorDisplaySetting[start]== 1)|(file$SCC.MirrorDisplaySetting[start] == 2)){
      break}
  }
  
  if (start == nrow(file)){ ## no more event
    return (c(FALSE, FALSE))
  }
  
  # search end
  for (end in (start + 1):nrow(file)) {
    if (file$SCC.MirrorDisplaySetting[end]== 0){
      break}
  }
  
  if ((end - start) >= 300){
    return (c(-1,end))
  }else return (c(start, end))
}

mirrorEngagement <- function(file, start, end){
  speedmirror <- file$VDS.Veh.Speed[start:end]
  smirror <- cpts(cpt.meanvar(speedmirror, method = 'AMOC'))
  lanemirror <- file$SCC.Lane.Deviation.2[start:end]
  lmirror <- cpts(cpt.meanvar(lanemirror, method = 'AMOC'))
  #using minimum of the meanvar of speed and lanedevation
  engage <- min(c(smirror, lmirror))
  return (engage)
}

#creating a dataframe called eventTimesmirror to store the times
disp <- read_excel("H:\\disposition.xls")
disp <- filter(disp, disp$Analyze == "X" | disp$Reduced == "X")
disp$DaqName <- str_replace(disp$DaqName, ".daq", ".csv")
disp$ID <- as.numeric(substr(disp$DaqPath,1,3))
fileNames <- disp$DaqName
fileNames [155]
eventTimesmirror <- NULL
#finding start engagement and end for all the files and storing it
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventnumber <- 1
  while(done){
    start <- 1 
    if (eventnumber != 1){
      start <- end + 1
    }
    times <- mirrorDetect(file, start)
    if (times[1] == FALSE){
      done <- FALSE
    }else if (times[1] != -1){ # valid side mirror task
      start <- times[1]
      end <- times [2]
      engage <- mirrorEngagement(file, start, end)
      eventTimesmirror <- rbind(eventTimesmirror, c(disp$ID[i], as.character(fileNames[i]), as.character(disp$DosingLevel[i]),
                                                               eventnumber, start, (start+engage), end, (end-start)))
    }
    end <- times [2]
    eventnumber <- eventnumber + 1
  }
}

#renaming columns in the dataframe and writing dataframe into a csv file
eventTimesmirror <- as.data.frame(eventTimesmirror)
colnames(eventTimesmirror) <- c("ID", "DaqName", "DosingLevel", "eventNum", "start", "engagement", "end", "total")
eventTimesmirror$ID <- as.numeric(as.character(eventTimesmirror$ID))
eventTimesmirror$start <- as.numeric(as.character(eventTimesmirror$start))
eventTimesmirror$engagement <- as.numeric(as.character(eventTimesmirror$engagement))
eventTimesmirror$end <- as.numeric(as.character(eventTimesmirror$end))
eventTimesmirror$total <- as.numeric(as.character(eventTimesmirror$total))
eventTimesmirror <- eventTimesmirror[order(eventTimesmirror$ID, eventTimesmirror$DosingLevel),]
write.csv(eventTimesmirror, file = "H:\\NIDA\\eventTimesMirror.csv", row.names=FALSE)

#creating a matrix with the output variables for experimental and control group
#creating an  empty dataframe for analyzing the data
analysisMatrixmirror <- NULL
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventNum <- 1
  while(done){
    start <- 1 
    if (eventNum != 1){
      start <- end + 1
    }
    times <- mirrorDetect(file, start)
    if (times[1] == FALSE){
      done <- FALSE
    }else if (times[1] != -1){ # valid side mirror task
      start <- times[1]
      end <- times [2]
      #finding output for experimental group
      exp <- 1 
      SdLanemirror <- sd(file$SCC.Lane.Deviation.2[start:end])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[start:end])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[start:end])
      analysisMatrixmirror <- rbind(analysisMatrixmirror,
                                    c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                      exp, eventNum, SdLanemirror, avgSpeedmirror, sdSpeedmirror))
      #output for control group
      exp <- 0
      SdLanemirror <- sd(file$SCC.Spline.Lane.Deviation.2[(start - (end - start)):start])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[((start) - (end - start)):start])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[(start - (end - start)):start])
      
      analysisMatrixmirror <- rbind(analysisMatrixmirror,
                                    c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                      exp, eventNum, SdLanemirror, avgSpeedmirror, sdSpeedmirror)) 
    }
    end <- times [2]
    eventNum <- eventNum + 1
  }
}

#renaming columns and writing to a csv
analysisMatrixmirror <- as.data.frame(analysisMatrixmirror)
colnames(analysisMatrixmirror) <- c("ID", "DaqName", "DosingLevel", "Experiment",
                                    "eventNum", "SD.Lane.Deviation", "Avg.Speed", "Sd.Speed")
analysisMatrixmirror$ID <- as.numeric(as.character(analysisMatrixmirror$ID))
analysisMatrixmirror$SD.Lane.Deviation <- as.numeric(as.character(analysisMatrixmirror$SD.Lane.Deviation))
analysisMatrixmirror$Avg.Speed <- as.numeric(as.character(analysisMatrixmirror$Avg.Speed))
analysisMatrixmirror$Sd.Speed <- as.numeric(as.character(analysisMatrixmirror$Sd.Speed))
analysisMatrixmirror <- analysisMatrixmirror[order(analysisMatrixmirror$ID, analysisMatrixmirror$DosingLevel),]
write.csv(analysisMatrixmirror, file = "H:\\NIDA\\analysisMirror.csv", row.names=FALSE)
