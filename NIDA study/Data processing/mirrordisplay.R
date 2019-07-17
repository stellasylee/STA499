library(stringr)
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

#creating a matrix with the output variables for experimental and control group
#creating an  empty dataframe for analyzing the data
analysisMirror <- NULL
eventR <- 1
#finding start engagement and end for all the files and storing it
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  print(fileNames[i])
  done <- TRUE
  eventNum <- 1
  while(eventNum <= 14){
    if (done){
      start <- 1 
      if (eventNum != 1){
        start <- end + 1
      }
      if ((eventNum == 1) & (disp$Restart[i] != "RNA")) { # Restart file
        eventNum <- (1 + as.numeric(as.character(analysisMirror$'X.1..1'[(eventR - 1)])))
        if (eventNum > 14){
          done <- FALSE
          break
        }
      }
      times <- mirrorDetect(file, start)
      if (times[1] == FALSE){
        done <- FALSE
      }else if (times[1] > 10){ # valid side mirror task 
        start <- times[1]
        end <- times [2]
        valid <- times [3]
        print(paste("start: ", start, "end", end))
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

        #output for experiment group
        exp <- 1 
        SdLanemirror <- sd(file$SCC.Lane.Deviation.2[start:end])
        avgSpeedmirror <- mean(file$VDS.Veh.Speed[start:end])
        sdSpeedmirror <- sd(file$VDS.Veh.Speed[start:end])
        analysisMirror <- rbind.data.frame(analysisMirror,
                                      c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]), disp$Visit[i],
                                        exp, eventNum, start, end, (end - start), 
                                        SdLanemirror, avgSpeedmirror, sdSpeedmirror,
                                        valid, cannabis, alcohol, place), 
                                      stringsAsFactors = FALSE)
        #output for control group
        exp <- 0
        SdLanemirror <- sd(file$SCC.Spline.Lane.Deviation.2[(start - (end - start)):start])
        avgSpeedmirror <- mean(file$VDS.Veh.Speed[((start) - (end - start)):start])
        sdSpeedmirror <- sd(file$VDS.Veh.Speed[(start - (end - start)):start])
        
        analysisMirror <- rbind.data.frame(analysisMirror,
                                      c(disp$ID[i], paste0(fileNames[i]), paste0(disp$DosingLevel[i]), disp$Visit[i],
                                        exp, eventNum, start, end, (end - start), 
                                        SdLanemirror, avgSpeedmirror, sdSpeedmirror,
                                        valid, cannabis, alcohol, place), 
                                      stringsAsFactors = FALSE)
        eventR <- eventR + 2
      }
    }
    end <- times [2]
    eventNum <- eventNum + 1
  }
}
analysisMirror
#renaming columns and writing to a csv
colnames(analysisMirror) <- c("ID", "DaqName", "DosingLevel", "Visit", "Experiment",
                              "eventNum", "start", "end", "total",
                              "SD.Lane.Deviation", "Avg.Speed", "SD.Speed",
                              "valid", "THC", "BAC", "LogStreams.5")
# Change instances with 299 completion time to invalid
analysisMirror$valid<-ifelse(analysisMirror$total == 299, 0, analysisMirror$valid)
analysisMirror$ID <- as.numeric(as.character(analysisMirror$ID))
analysisMirror$SD.Lane.Deviation <- as.numeric(as.character(analysisMirror$SD.Lane.Deviation))
analysisMirror$Avg.Speed <- as.numeric(as.character(analysisMirror$Avg.Speed))
analysisMirror$SD.Speed <- as.numeric(as.character(analysisMirror$SD.Speed))
analysisMirror <- analysisMirror[order(analysisMirror$ID, analysisMirror$DosingLevel),]
write.csv(analysisMirror, file = "H:\\NIDA\\analysisMirror.csv", row.names=FALSE)
