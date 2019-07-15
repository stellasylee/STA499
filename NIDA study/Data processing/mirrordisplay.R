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
    return (c(FALSE, FALSE, FALSE))
  }
  
  # search end
  for (end in (start + 1):nrow(file)) {
    if (file$SCC.MirrorDisplaySetting[end]== 0){
      break}
  }
  
  if ((end - start) >= 300){
    valid <- FALSE
    return (c(start, end, valid))
  }else {
    valid <- TRUE
    return (c(start, end, valid))
  }
}

#creating a dataframe called eventTimesmirror to store the times
disp <- read_excel("H:\\disposition.xls")
disp <- filter(disp, disp$Analyze == "X" | disp$Reduced == "X")
disp$DaqName <- str_replace(disp$DaqName, ".daq", ".csv")
disp$ID <- as.numeric(substr(disp$DaqPath,1,3))
fileNames <- disp$DaqName
dosing <- read.csv("H:\\NIDA\\DosingLevelNIDA.csv")
# Join correct dosing levels to disposition file
dosingF <- dosing [, -(5:41)]
colnames(dosingF)[1] <- "ID"
disp$Visit <- as.numeric(disp$Visit)
newDisp <- left_join(x = disp, y = dosingF, by = c("ID","Visit"))

eventTimesmirror <- NULL
eventR <- 1
#finding start engagement and end for all the files and storing it
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventNum <- 1
  while(eventNum <= 14){
    if (done){
    start <- 1 
    if (eventNum != 1){
      start <- end + 1
    }
    if ((eventNum == 1) & (disp$Restart[i] != "RNA")) { # Restart file
      eventNum <- (1 + as.numeric(as.character(eventTimesmirror$'X.1.'[(eventR - 1)])))
      if (eventNum > 14){
        done <- FALSE
        break
      }
    }
    times <- mirrorDetect(file, start)
    if (times[1] == FALSE){
      done <- FALSE
    }else { # valid side mirror task
      start <- times[1]
      end <- times [2]
      valid <- times [3]
      # Find appropriate dosing condition for data
      place <- file$SCC.LogStreams.5[start] # LogStream information 
      dosingInfo <- newDisp [which(newDisp$ID == file$ID[1]),] %>% .[which(.$DosingLevel == file$DosingLevel[1]),]
      if (place %in% 11:14){ # urban segment
        cannabis <- dosingInfo$THC_Urban[1]
        alcohol <- dosingInfo$BAC_Urban[1]
      }else if (place %in% 21:23){ # interstate
        cannabis <- dosingInfo$THC_Interstate[1]
        alcohol <- dosingInfo$BAC_Interstate[1]
      }else if (place %in% 31:35){ # rural
        cannabis <- dosingInfo$THC_Rural[1]
        alcohol <- dosingInfo$BAC_Rural[1]
      }else if (place == 36){ # rural straight
        cannabis <- dosingInfo$THC_RuralStraight[1]
        alcohol <- dosingInfo$BAC_RuralStraight[1]
      }else print(fileNames[i]) # error 
      eventTimesmirror <- rbind.data.frame(eventTimesmirror, 
                                           c(disp$ID[i], as.character(fileNames[i]), as.character(disp$DosingLevel[i]),
                                             eventNum, start, end, (end-start),
                                             valid, cannabis, alcohol, place), 
                                           stringsAsFactors = FALSE)
      eventR <- eventR + 1
    }
    }
    eventNum <- eventNum + 1
  }
}

#renaming columns in the dataframe and writing dataframe into a csv file
colnames(eventTimesmirror) <- c("ID", "DaqName", "DosingLevel", "eventNum", "start", "end", "total",
                                "valid", "THC", "BAC", "LogStreams.5")
eventTimesmirror$ID <- as.numeric(as.character(eventTimesmirror$ID))
eventTimesmirror$start <- as.numeric(as.character(eventTimesmirror$start))
eventTimesmirror$engagement <- as.numeric(as.character(eventTimesmirror$engagement))
eventTimesmirror$end <- as.numeric(as.character(eventTimesmirror$end))
eventTimesmirror$total <- as.numeric(as.character(eventTimesmirror$total))
eventTimesmirror <- eventTimesmirror[order(eventTimesmirror$ID, eventTimesmirror$DosingLevel),]
write.csv(eventTimesmirror, file = "H:\\NIDA\\mirrorTimes.csv", row.names=FALSE)

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
