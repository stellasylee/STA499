install.packages("readxl")
install.packages("tidyverse")
install.packages("foreach")
install.packages("changepoint")

library(changepoint)
library(rio)
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(stringr) # String manipulation
library(tidyverse)


files <- list.files("H:\\CannabisStudy\\CSV")

#reading in the first file

m1 <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[1]))

#reading in the disposition and set up task files

setup <- read_excel("H:/CannabisStudy/setupTask.xls")

disp <- read.csv("H:\\CannabisStudy\\dispositionUpdate.csv")
files <- disp$DaqName

#checking whether the participant completed the task or not
#filtering to keep only values with 1

mone <- filter(m1, m1$SCC.MirrorDisplaySetting == "1")

length(mone)

which(m1$SCC.MirrorDisplaySetting == 1)

which(m1$SCC.MirrorDisplaySetting == 2)


#writing function to detect the start and event of side mirror task

mirrordetect <- function (file, s){
  for (start in s:nrow(file)) {
    if ((file$SCC.MirrorDisplaySetting[start]== 1)| (file$SCC.MirrorDisplaySetting[start] == 2)){
      break}
  }
  
  if (start == nrow(file)){
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


#testing which changepoint to use

changepointtest <- read.csv("H:\\CannabisStudy\\CSV\\20131207122954.csv")

mirrordetect(changepointtest, 1)

plot(cpt.meanvar(changepointtest$VDS.Veh.Speed[1:6801], method = 'AMOC'))
plot(cpt.mean(changepointtest$VDS.Veh.Speed[6311:6515], method = 'AMOC'))
plot(cpt.var(changepointtest$VDS.Veh.Speed[6311:6515], method = 'AMOC'))


MirrorEngagement <- function(file, start, end){
  speedmirror <- file$VDS.Veh.Speed[start:end]
  smirror <- cpts(cpt.meanvar(speedmirror, method = 'AMOC'))
  lanemirror <- file$SCC.Lane.Deviation.2[start:end]
  lmirror <- cpts(cpt.meanvar(lanemirror, method = 'AMOC'))
  #using minimum of the meanvar of speed and lanedevation
  engagemirror <- min(c(smirror, lmirror))
  return(engage)
  
}
library(stringr)
#creating a dataframe called eventTimesmirror to store the times
disp <- read_excel("H:/CannabisStudy/disposition.xls")
disp <- filter(dispo, dispo$Analyze == "X" | dispo$Reduced == "X")
disp$DaqName <- str_replace(disp$DaqName, ".daq", ".csv")
disp$ID <- as.numeric(substr(disp$DaqPath,1,3))
files <- disp$DaqName

eventTimesmirror <- data.frame(matrix(ncol = 8, nrow = 0))

#finding start engagement and end for all the files and storing it
for (i in 1:length(files)){
  file <- read.csv(paste0("H:\\CannabisStudy\\SideMirrorCSV\\", files[i]))
  done <- TRUE
  eventnumber <- 1
  while(done){
    start <- 1 
    if (eventnumber != 1){
      start <- end + 1
    }
    times <- mirrordetect(file, start)
    if (times[1] == FALSE){
      done <- FALSE
    }else if (times[1] != -1){
      # calculation / add to our dataframe
      start <- times[1]
      end <- times [2]
      engage <- MirrorEngagement(file, start, end)
      eventTimesmirror <- rbind.data.frame(eventTimesmirror, c(disp$ID[i], as.character(fileNames[i]),
          as.character(disp$DosingLevel[i]), eventnumber, start, engage, end, (end-start)),
          stringsAsFactors = FALSE)
    }
    
    end <- times [2]
    eventnumber <- eventnumber + 1
  }
}

#renaming columns in the dataframe and writing dataframe into a csv file

colnames(eventTimesmirror) <- c("ID", "DaqName", "DosingLevel", "eventNum", "start", "engagement", "end", "total")
eventTimesmirror$ID <- as.numeric(eventTimesmirror$ID)
eventTimesmirror$start <- as.numeric(eventTimesmirror$start)
eventTimesmirror$engagement <- as.numeric(eventTimesmirror$engagement)
eventTimesmirror$end <- as.numeric(eventTimesmirror$end)
eventTimesmirror$total <- as.numeric(eventTimesmirror$total)
eventTimesmirror <- eventTimesmirror[order(eventTimesmirror$ID, eventTimesmirror$DosingLevel),]
write.csv(eventTimesmirror, file = "H:\\CannabisStudy\\eventTimesmirror.csv", row.names=FALSE)

#creating a matrix with the output variables for experimental and control group

#creating an  empty dataframe for analyzing the data

analysisMatrixmirror <- NULL

for (i in 1:length(files)){
  file <- read.csv(paste0("H:\\CannabisStudy\\SideMirrorCSV\\", files[i]))
  done <- TRUE
  eventnumber <- 1
  # done <- FALSE when there is no more event in this file
  while(done){
    # update the start as the end of last event
    start <- 1 
    if (eventnumber != 1){
      start <- end + 1
    }
    # detect mirror event
    times <- mirrordetect(file, start)
    if (times[1] == FALSE){ # no more event
      done <- FALSE
    }else if (times[1] != -1){ # check valid event
      start <- times[1]
      end <- times [2]
      engage <- MirrorEngagement(file, start, end)
    
      #finding output for experimental group
      exp <- 1 
      SdLanemirror <- sd(file$SCC.Lane.Deviation.2[engage:end])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[engage:end])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[engage:end])
      analysisMatrixmirror <- rbind(analysisMatrixmirror,
                                    c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                    exp, eventnumber, SdLanemirror, avgSpeedmirror, sdSpeedmirror))
      #output for control group
      exp <- 0
      SdLanemirror <- sd(file$SCC.Spline.Lane.Deviation.2[((start) - (end - engage)):start])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[((start) - (end - engage)):start])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[((start) - (end - engage)):start])
      
      analysisMatrixmirror <- rbind(analysisMatrixmirror,
                                    c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                      exp, eventnumber, SdLanemirror, avgSpeedmirror, sdSpeedmirror)) 
      
    }
    
    end <- times [2]
    eventnumber <- eventnumber + 1
    
  }
}

#renaming columns and writing to a csv

colnames(analysisMatrixmirror) <- c("ID", "FileName", "DosingLevel", "experiment", "eventnumber", "SdLanemirror", "avgSpeedmirror", "sdSpeedmirror")
analysisMatrixmirror$ID <- as.numeric(analysisMatrixmirror$ID)
analysisMatrixmirror$start <- as.numeric(analysisMatrixmirror$start)
analysisMatrixmirror$engagement <- as.numeric(analysisMatrixmirror$engagement)
analysisMatrixmirror$end <- as.numeric(analysisMatrixmirror$end)
analysisMatrixmirror$total <- as.numeric(analysisMatrixmirror$total)
analysisMatrixmirror <- analysisMatrixmirror[order(analysisMatrixmirror$ID, analysisMatrixmirror$DosingLevel),]
write.csv(analysisMatrixmirror, file = "H:\\CannabisStudy\\analysisMatrixmirror.csv", row.names=FALSE)
