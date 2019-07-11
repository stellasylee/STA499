install.packages("readxl")
install.packages("tidyverse")
install.packages("foreach")

library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(stringr) # String manipulation
library(tidyverse)

#checking if the events occurred on the interstate or not

files <- list.files("H:\\CannabisStudy\\CSV")

m1 <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[1]))


#reading in file with three event times
eventTimes3 <- read.csv("H:\\CannabisStudy\\eventTimesWith3.csv")


#finding the logstream value of the file and adding it to the df

#if logstream value == 22, then it is the interstate

  filen <- eventTimes3$DaqName
  eventTimes3[,"logstream"]  <- NA
  for (index in 1:nrow(eventTimes3)) {
    file <- read.csv(paste0("H:\\CannabisStudy\\CSV\\", filen[i]))
    starttask <- as.numeric(eventTimes3$start[index])
    interstate <- file$SCC.LogStreams.5[starttask]
    eventTimes3[index, "logstream"] <- interstate
    
  }

#writing event times to csv
  
write.csv(eventTimes3, file = "H:\\CannabisStudy\\checkinginterstate.csv", row.names=FALSE)
