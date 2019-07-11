library(stringr)
library(tidyverse)
library ("readxl")
library ("dplyr")

times <- read.csv("H:\\NIDA\\eventTimesWith3.csv")
times <- times[-which(times$DaqName %in% c("20130612092744.csv", "20140104105417.csv")),]
outputs <- read.csv("H:\\NIDA\\analysisTasks3.csv")
dosing <- read.csv("H:\\NIDA\\DosingLevelNIDA.csv")

# Join correct dosing levels to disposition file
dosingF <- dosing [, -(5:41)]
colnames(dosingF)[1] <- "ID"
disp <- read.csv("H:\\NIDA\\dispositionUpdate.csv")
newDisp <- left_join(x = disp, y = dosingF, by = c("ID","Visit"))

fileNames <- times$DaqName

## Add variable for page number and valid (not time-out), dosing level at task time
times[,"pageNum"] <- NA
outputs[,"pageNum"] <- NA
times[,"valid"] <- NA
outputs[,"valid"] <- NA
times[,"THC"] <- NA
outputs[,"THC"] <- NA
times[,"BAC"] <- NA
outputs[,"BAC"] <- NA
times[,"LogStreams.5"] <- NA
outputs[,"LogStreams.5"] <- NA
j <- 1
for (i in 1:length(fileNames)){
  file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  start <- times$start[i]
  end <- times$end[i]
  print (paste0(start, " ", end))
  # Add corresponding page number by MenuSearch value
  ## 13:15 is not present in simulation
  page <-  switch(file$SCC.MenuSearch[start],
                  1, 2, 3, 1, 2, 3, 2, 2, 2, 1, 1, 3,
                  -1, -1, -1, 1, 2, 3, 1, 1, 2)
  # Find the task was valid
  if (file$AUX1.MenuScore[end] == 1){
    v <- 1
  }else v <- 0
  
  # Find appropriate dosing level
  place <- file$SCC.LogStreams.5[start]
  
  # Find position in disposition file
  doingInfo <- newDisp [which(newDisp$ID == f$ID[i]),] %>% .[which(.$Visit == f$Visit[i]),17:24]
  if (place %in% 11:14){ # urban segment
    cannabis <- doingInfo$THC_Urban[1]
    alcohol <- doingInfo$BAC_Urban[1]
  }else if (place %in% 21:23){ # interstate
    cannabis <- doingInfo$THC_Interstate[1]
    alcohol <- doingInfo$BAC_Interstate[1]
  }else if (place %in% 31:35){ # rural
    cannabis <- doingInfo$THC_Rural[1]
    alcohol <- doingInfo$BAC_Rural[1]
  }else if (place == 36){ # rural straight
    cannabis <- doingInfo$THC_RuralStraight[1]
    alcohol <- doingInfo$BAC_RuralStraight[1]
  }else print(fileNames[i]) # error 
  
  times <- assignValues(times, i, page, v, place, cannabis, alcohol)
  outputs <- assignValues(outputs, j, page, v, place, cannabis, alcohol)
  outputs <- assignValues(outputs, j+1, page, v, place, cannabis, alcohol)
  j <- j + 2
}

assignValues <- function(df, i, page, v, place, cannabis, alcohol){
  df[i,"pageNum"] <- page
  df[i,"valid"] <- v
  df[i,"LogStreams.5"] <- place
  df[i, "THC"] <- cannabis
  df[i, "BAC"] <- alcohol
  return (df)
}
