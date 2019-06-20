#install.packages("R.matlab")
library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)

## Grab all of the .mat Files from directory H:\V2VMAT\MatFiles
files <- list.files("H:\\V2VMAT\\MatFiles")
disp <- readxl::read_excel("H:\\disposition.xls")
rand <- readxl::read_excel("H:\\MainTestMatrix.xlsx")


## For each participant, we will get the participant first response
## find haptic alert start time
findAlert <- function (data) {
  haptic <- which(rownames(data$elemDataI) == 'SCC.Haptic.Alert')
  h <- data$elemDataI[[haptic]][,1]
  for (alert in 1: 13349){
    if (h[alert] != 0){
      alert <- numeric(alert)
      break
    }
  }
  return (alert)
}

for (i in (1:30)){
  test <- readMat(paste0("/Users/seoyeon/Documents/2019MAP/MatFiles/",file40[i]))
  start <- findAlert (test)
}
