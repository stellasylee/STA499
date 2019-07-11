library(changepoint)
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(tidyverse)

#writing function to detect the start and event of side mirror task
messageDetect <- function (file, s, scenario, eventNum){
  scen <- switch(scenario,
                    c(5, 2, 3, 4, 6, 1), #S1A
                    c(7, 10, 8, 9, 12, 11), #S1B
                    c(19, 20, 21, 22, 24, 23), #S2A
                    c(13, 14, 15, 16, 17, 18), #S2B
                    c(25, 29, 26, 28, 27, 30), #S3A
                    c(35, 32, 31, 34, 33, 36)) #S3B
  correct <- scen[eventNum]
  print(correct)
  for (start in s:nrow(file)) {
    if (file$SCC.MessageReading[start] == correct){
      break}
  }
  
  end <- start + 600 # after 10 seconds
  
  if ((start == nrow(file))|(end > nrow(file))){ # no more event
    return (c(FALSE))
  }else return (c(start, end, file$SCC.MessageReading[start]))
}

messageEngagement <- function(file, start, end){
  speedmes <- file$VDS.Veh.Speed[start:end]
  smes <- cpts(cpt.meanvar(speedmes, method = 'AMOC'))
  lanemes <- file$SCC.Lane.Deviation.2[start:end]
  lmes <- cpts(cpt.meanvar(lanemes, method = 'AMOC'))
  #using minimum of the meanvar of speed and lanedevation
  engage <- min(c(smes, lmes))
  return (engage)
}

#creating a dataframe called eventTimesmirror to store the times
disp <- read_excel("H:\\disposition.xls")
disp <- filter(disp, disp$Analyze == "X" | disp$Reduced == "X")
disp$DaqName <- str_replace(disp$DaqName, ".daq", ".csv")
disp$ID <- as.numeric(substr(disp$DaqPath,1,3))
fileNames <- disp$DaqName

eventTimesMes <- NULL
#finding start engagement and end for all the files and storing it
eventR <- 1
for (i in 7:10){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  for (eventNum in 1:6){
    print (paste0( eventNum, ":"))
    if (done){
    start <- 1 
    if (eventNum != 1){
      start <- end + 1
    }
    scen <- switch (disp$Drive[i], "S1A" = {1}, "S1B" = {2}, "S2A" = {3}, "S2B" = {4}, "S3A" = {5}, "S3B" = {6})
    if ((eventNum == 1) & (disp$Restart[i] != "RNA")) { # Restart file
      eventNum <- (1 + as.numeric(as.character(eventTimesMes$'X.1.'[(eventR - 1)])))
      print (paste0(eventR, " :::::::", eventNum))
    }
    times <- messageDetect(file, start, scen, eventNum)
    if (times[1] == FALSE){
      done <- FALSE
    }else { # valid message reading task
      start <- times[1]
      end <- times [2]
      messageVal <- times[3]
      engage <- messageEngagement(file, start, end)
      eventTimesMes <- rbind.data.frame(eventTimesMes,
                                        c(disp$ID[i], as.character(fileNames[i]), as.character(disp$DosingLevel[i]),
                                          eventNum, messageVal, start, (start+engage), end, (end-start)),
                                        stringsAsFactors = FALSE)
      eventR <- eventR + 1
    }
    print (paste0(eventR, " after setting", eventNum))
    eventNum <- eventNum + 1
    }
  }
}

#renaming columns in the dataframe and writing dataframe into a csv file
colnames(eventTimesMes) <- c("ID", "DaqName", "DosingLevel", "eventNum", "message", "start", "engagement", "end", "total")
eventTimesMes$ID <- as.numeric(as.character(eventTimesMes$ID))
eventTimesMes$start <- as.numeric(as.character(eventTimesMes$start))
eventTimesMes$engagement <- as.numeric(as.character(eventTimesMes$engagement))
eventTimesMes$end <- as.numeric(as.character(eventTimesMes$end))
eventTimesMes$total <- as.numeric(as.character(eventTimesMes$total))
eventTimesMes <- eventTimesMes[order(eventTimesMes$ID, eventTimesMes$DosingLevel),]
write.csv(eventTimesMes, file = "H:\\NIDA\\eventTimesMessage.csv", row.names=FALSE)

#creating a matrix with the output variables for experimental and control group
#creating an  empty dataframe for analyzing the data
analysisMatrixMesNC <- NULL
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventnumber <- 1
  while(done){
    start <- 1 
    if (eventnumber != 1){
      start <- end + 1
    }
    scen <- switch (disp$Drive[i], "S1A" = {1}, "S1B" = {2}, "S2A" = {3}, "S2B" = {4}, "S3A" = {5}, "S3B" = {6})
    times <- messageDetect(file, start, scen)
    if (times[1] == FALSE){
      done <- FALSE
    }else { # valid message reading task
      start <- times[1]
      end <- times [2]
      messageVal <- times[3]
      engage <- messageEngagement(file, start, end)
      #finding output for experimental group
      exp <- 1 
      SdLanemirror <- sd(file$SCC.Lane.Deviation.2[start:end])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[start:end])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[start:end])
      analysisMatrixMesNC <- rbind.data.frame(analysisMatrixMesNC,
                                    c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                      exp, eventNum, messageVal, SdLanemirror, avgSpeedmirror, sdSpeedmirror))
      #output for control group
      exp <- 0
      SdLanemirror <- sd(file$SCC.Spline.Lane.Deviation.2[(start - (end - start)):start])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[((start) - (end - start)):start])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[(start - (end - start)):start])
      analysisMatrixMesNC <- rbind.data.frame(analysisMatrixMesNC,
                                    c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                      exp, eventNum, messageVal, SdLanemirror, avgSpeedmirror, sdSpeedmirror)) 
    }
    eventnumber <- eventnumber + 1
  }
}

#renaming columns and writing to a csv
colnames(analysisMatrixmirror) <- c("ID", "DaqName", "DosingLevel", "Experiment",
                                    "eventNum", "message", "SD.Lane.Deviation", "Avg.Speed", "Sd.Speed")
analysisMatrixmirror$ID <- as.numeric(as.character(analysisMatrixmirror$ID))
analysisMatrixmirror$SD.Lane.Deviation <- as.numeric(as.character(analysisMatrixmirror$SD.Lane.Deviation))
analysisMatrixmirror$Avg.Speed <- as.numeric(as.character(analysisMatrixmirror$Avg.Speed))
analysisMatrixmirror$Sd.Speed <- as.numeric(as.character(analysisMatrixmirror$Sd.Speed))
analysisMatrixmirror <- analysisMatrixmirror[order(analysisMatrixmirror$ID, analysisMatrixmirror$DosingLevel),]
write.csv(analysisMatrixmirror, file = "H:\\NIDA\\analysisMesNC.csv", row.names=FALSE)


#creating a matrix with the output variables for experimental and control group
#creating an  empty dataframe for analyzing the data
analysisMatrixMesCP <- NULL
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventnumber <- 1
  while(done){
    start <- 1 
    if (eventnumber != 1){
      start <- end + 1
    }
    scen <- switch (disp$Drive[i], "S1A" = {1}, "S1B" = {2}, "S2A" = {3}, "S2B" = {4}, "S3A" = {5}, "S3B" = {6})
    times <- messageDetect(file, start, scen)
    if (times[1] == FALSE){
      done <- FALSE
    }else { # valid message reading task
      start <- times[1]
      end <- times [2]
      messageVal <- times[3]
      engage <- messageEngagement(file, start, end)
      #finding output for experimental group
      exp <- 1 
      SdLanemirror <- sd(file$SCC.Lane.Deviation.2[engage:end])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[engage:end])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[engage:end])
      analysisMatrixMesCP <- rbind.data.frame(analysisMatrixMesCP,
                                   c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                     exp, eventNum, messageVal, SdLanemirror, avgSpeedmirror, sdSpeedmirror))
      #output for control group
      exp <- 0
      SdLanemirror <- sd(file$SCC.Spline.Lane.Deviation.2[(start - (end - engage)):start])
      avgSpeedmirror <- mean(file$VDS.Veh.Speed[((start) - (end - engage)):start])
      sdSpeedmirror <- sd(file$VDS.Veh.Speed[(start - (end - engage)):start])
      analysisMatrixMesCP <- rbind.data.frame(analysisMatrixMesCP,
                                   c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                     exp, eventNum, messageVal, SdLanemirror, avgSpeedmirror, sdSpeedmirror)) 
    }
    eventnumber <- eventnumber + 1
  }
}

#renaming columns and writing to a csv
colnames(analysisMatrixMesCP) <- c("ID", "DaqName", "DosingLevel", "Experiment",
                                    "eventNum", "message", "SD.Lane.Deviation", "Avg.Speed", "Sd.Speed")
analysisMatrixMesCP$ID <- as.numeric(as.character(analysisMatrixMesCP$ID))
analysisMatrixMesCP$SD.Lane.Deviation <- as.numeric(as.character(analysisMatrixMesCP$SD.Lane.Deviation))
analysisMatrixMesCP$Avg.Speed <- as.numeric(as.character(analysisMatrixMesCP$Avg.Speed))
analysisMatrixMesCP$Sd.Speed <- as.numeric(as.character(analysisMatrixMesCP$Sd.Speed))
analysisMatrixMesCP <- analysisMatrixMesCP[order(analysisMatrixMesCP$ID, analysisMatrixMesCP$DosingLevel),]
write.csv(analysisMatrixmirror, file = "H:\\NIDA\\analysisMesCP.csv", row.names=FALSE)

for (i in 1:6){
  if (i == 3){
    i <- 5
  }
  print (i)
}
