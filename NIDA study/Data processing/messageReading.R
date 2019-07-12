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
  if (eventNum == 1){
    valid <- FALSE
  }else valid <- TRUE
  correct <- scen[eventNum]
  print(correct)
  for (start in s:nrow(file)) {
    if ((valid == TRUE)&(file$SCC.MessageReading[start] == correct)){
      break
    }else if (file$SCC.MessageReading[start] == 0){
      valid <- TRUE
    }
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

dosing <- read.csv("H:\\NIDA\\DosingLevelNIDA.csv")
# Join correct dosing levels to disposition file
dosingF <- dosing [, -(5:41)]
colnames(dosingF)[1] <- "ID"
disp$Visit <- as.numeric(disp$Visit)
newDisp <- left_join(x = disp, y = dosingF, by = c("ID","Visit"))
eventTimesMes <- NULL
#finding start engagement and end for all the files and storing it
eventR <- 1
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventNum <- 1
  while (eventNum <= 6){
    if (done){
      start <- 1 
      if (eventNum != 1){
        start <- end + 1
      }
      scen <- switch (disp$Drive[i], "S1A" = {1}, "S1B" = {2}, "S2A" = {3}, "S2B" = {4}, "S3A" = {5}, "S3B" = {6})
      if ((eventNum == 1) & (disp$Restart[i] != "RNA")) { # Restart file
        eventNum <- (1 + as.numeric(as.character(eventTimesMes$'X.1.'[(eventR - 1)])))
        if (eventNum > 6){
          break
        }
      }
      times <- messageDetect(file, start, scen, eventNum)
      if (times[1] == FALSE){
        done <- FALSE
      }else { # valid message reading task
        start <- times[1]
        end <- times [2]
        messageVal <- times[3]
        engage <- messageEngagement(file, start, end)
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
        
        eventTimesMes <- rbind.data.frame(eventTimesMes,
                                          c(disp$ID[i], as.character(fileNames[i]), as.character(disp$DosingLevel[i]),
                                            eventNum, messageVal,
                                            start, (start+engage), end, (end-start), 
                                            cannabis, alcohol, place), 
                                          stringsAsFactors = FALSE)
        eventR <- eventR + 1
      }
    }
    eventNum <- eventNum + 1
  }
}

#renaming columns in the dataframe and writing dataframe into a csv file
colnames(eventTimesMes) <- c("ID", "DaqName", "DosingLevel", "eventNum", "message",
                             "start", "engagement", "end", "total",
                             "THC", "BAC", "LogStreams.5")
eventTimesMes$ID <- as.numeric(as.character(eventTimesMes$ID))
eventTimesMes$start <- as.numeric(as.character(eventTimesMes$start))
eventTimesMes$engagement <- as.numeric(as.character(eventTimesMes$engagement))
eventTimesMes$end <- as.numeric(as.character(eventTimesMes$end))
eventTimesMes$total <- as.numeric(as.character(eventTimesMes$total))
eventTimesMes$THC <- as.numeric(as.character(eventTimesMes$THC))
eventTimesMes$BAC <- as.numeric(as.character(eventTimesMes$BAC))
eventTimesMes <- eventTimesMes[order(eventTimesMes$ID, eventTimesMes$DosingLevel),]
write.csv(eventTimesMes, file = "H:\\NIDA\\eventTimesMessage.csv", row.names=FALSE)

#creating a matrix with the output variables for experimental and control group
#creating an  empty dataframe for analyzing the data
analysisMatrixMes <- NULL
eventR <- 1
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  done <- TRUE
  eventNum <- 1
  while (eventNum <= 6){
    if (done){
      start <- 1 
      if (eventNum != 1){
        start <- end + 1
      }
      scen <- switch (disp$Drive[i], "S1A" = {1}, "S1B" = {2}, "S2A" = {3}, "S2B" = {4}, "S3A" = {5}, "S3B" = {6})
      if ((eventNum == 1) & (disp$Restart[i] != "RNA")) { # Restart file
        eventNum <- (1 + as.numeric(as.character(analysisMatrixMes$'X.1..1'[(eventR - 1)])))
        #print(paste0("updated due to restart:", eventNum))
        if (eventNum > 6){
          break
        }
      }
      times <- messageDetect(file, start, scen, eventNum)
      if (times[1] == FALSE){
        done <- FALSE
      }else { # valid message reading task
        start <- times[1]
        end <- times [2]
        messageVal <- times[3]

        # Find appropriate dosing condition for data ----
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
        
        #finding output for experimental group ----
        exp <- 1 
        SdLanemirror <- sd(file$SCC.Lane.Deviation.2[start:end])
        avgSpeedmirror <- mean(file$VDS.Veh.Speed[start:end])
        sdSpeedmirror <- sd(file$VDS.Veh.Speed[start:end])
        analysisMatrixMes <- rbind.data.frame(analysisMatrixMes,
                                              c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                                exp, eventNum, messageVal, SdLanemirror, avgSpeedmirror, sdSpeedmirror, 
                                                cannabis, alcohol, place),
                                              stringsAsFactors = FALSE)
        #output for control group ----
        exp <- 0
        SdLanemirror <- sd(file$SCC.Lane.Deviation.2[(start - (end - start)):start])
        avgSpeedmirror <- mean(file$VDS.Veh.Speed[((start) - (end - start)):start])
        sdSpeedmirror <- sd(file$VDS.Veh.Speed[(start - (end - start)):start])
        analysisMatrixMes <- rbind.data.frame(analysisMatrixMes,
                                              c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]),
                                                exp, eventNum, messageVal, SdLanemirror, avgSpeedmirror, sdSpeedmirror, 
                                                cannabis, alcohol, place),
                                              stringsAsFactors = FALSE) 
        eventR <- eventR + 2
      }
    }
    eventNum <- eventNum + 1
  }
}

#renaming columns and writing to a csv
colnames(analysisMatrixMes) <- c("ID", "DaqName", "DosingLevel", "Experiment",
                                 "eventNum", "message", "SD.Lane.Deviation", "Avg.Speed", "Sd.Speed",
                                 "THC", "BAC", "LogStreams.5")
analysisMatrixMes$ID <- as.numeric(as.character(analysisMatrixMes$ID))
analysisMatrixMes$SD.Lane.Deviation <- as.numeric(as.character(analysisMatrixMes$SD.Lane.Deviation))
analysisMatrixMes$Avg.Speed <- as.numeric(as.character(analysisMatrixMes$Avg.Speed))
analysisMatrixMes$Sd.Speed <- as.numeric(as.character(analysisMatrixMes$Sd.Speed))
analysisMatrixMes$THC <- as.numeric(as.character(analysisMatrixMes$THC))
analysisMatrixMes$BAC <- as.numeric(as.character(analysisMatrixMes$BAC))
analysisMatrixMes <- analysisMatrixMes[order(analysisMatrixMes$ID, analysisMatrixMes$DosingLevel),]
write.csv(analysisMatrixMes, file = "H:\\NIDA\\analysisMes.csv", row.names=FALSE)

