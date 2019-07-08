install.packages("readxl")
install.packages("tidyverse")
install.packages("foreach")


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


#writing function to find the start and event of side mirror task

#####
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


#running mirror detect on all the files
for (i in 1:length(files)){
  file <- read.csv(paste0("H:\\CannabisStudy\\CSV\\", files[i]))
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
      print (paste0("valid start", start, " end ", end, " ", eventnumber))
    }
    end <- times [2]
    eventnumber <- eventnumber + 1
  }
}

#using changepoint methods to detect enagagement

MirrorEngagement <- function (file, start, end){
  speed <- file$VDS.Veh.Speed[start:end]
  s <- cpts(cpt.meanvar(speed, method = 'AMOC'))
  lane <- file$SCC.Lane.Deviation.2[start:end]
  l <- cpts(cpt.meanvar(lane, method = 'AMOC'))
  # our predicted engagement point is minimum between changepoint detection point in speed and lane deviation by mean and variance
  engage <- min(c(s,l))
  # Check this detected engagement point is before 'incorrect' answer if there was
    return (engage) 
}

#finding start, engagement, end and total time taken for side mirror task
