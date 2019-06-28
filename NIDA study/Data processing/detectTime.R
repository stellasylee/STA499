library(stringr) # String manipulation
library(tidyverse)
library ("readxl")
library ("dplyr")
library (change)

disp <- read.csv("H:\\NIDA\\dispositionUpdate.csv")
fileNames <- disp$DaqName

file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[1]))

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
