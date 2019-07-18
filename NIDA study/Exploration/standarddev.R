#code for finding standard deviation of lane deviation and mean speed
install.packages("readxl")
install.packages("tidyverse")
install.packages("R.matlab")
install.packages("R.matlab")
install.packages("doParallel")
install.packages("foreach")
install.packages("rio")
install.packages("changepoint")

library(rio)
library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(stringr) # String manipulation
library(tidyverse)
library (changepoint)




#trying to figure out how you find which rows would comprise the control group if we have the experimental group
#start <- (start of experimental group -(length(end experimental - start experimental)))
#end <- (start of the experimental group)

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
    if ((valid == TRUE) && (file$AUX1.MenuScore[end] == 1)){
      break
    }else if (file$AUX1.MenuScore[end] == 0){
      valid <- TRUE
    }
  }
  return (c(start, end))
}

detectEngagement <- function (start, end){
  
}

#code to calculate sd would be
#sd(m1$SCC.Lane.Deviation.2[start:end])
#writing this as a function

#sdlanedev <- function(file){
# for (i in length(all the files)) {
#  sd(m1$SCC.Lane.Deviation.2[start:end])

#}
#}

sdonefile <- sd(m1$SCC.Lane.Deviation.2[detectTime(m1)])

#function for standard deviation
sdlanedev <- function (file){
  return (sd(file$SCC.Lane.Deviation.2[detectTime(file)]))
}


for (i in 1:length(files)){
  testfile <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[i]))
  output <- sdlanedev(testfile)
  print(output)
}

#function for mean speed
meanspeed <- function(file){
  return(mean(file$VDS.Veh.Speed[detectTime(file)]))
}

for (i in 1:length(files)){
  testfile <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[i]))
  output <- meanspeed(testfile)
  print(output)
}

#function for standard deviation of speed

sdspeed <- function(file){
  return(sd(file$VDS.Veh.Speed[detectTime(file)]))
}

for (i in 1:length(files)){
  testfile <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[i]))
  output <- sdspeed(testfile)
  print(output)
}

