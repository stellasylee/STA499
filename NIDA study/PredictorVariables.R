install.packages("readxl")
install.packages("tidyverse")
install.packages("R.matlab")
install.packages("R.matlab")
install.packages("doParallel")
install.packages("foreach")
install.packages("rio")

library(rio)
library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(readxl) 
library(dplyr)

#grabbing the reduced csv files

files <- list.files("H:\\CannabisStudy\\CSV")

#reading in the first file

m1 <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[1]))

#reading in the disposition and set up task files

setup <- read_excel("H:/CannabisStudy/setupTask.xls")

dispo <- read_excel("H:/CannabisStudy/dispo.xls")

#filtering disposition file to keep only analyzed and reduced files

disposition <- filter(dispo, dispo$Analyze == "X" | dispo$Reduced == "X")

#getting the names of the files from the disposition file
filenames <- str_replace(disposition$DaqName, ".daq", ".csv")

#checking files which do not have unique values

#checking which files have  4 events

for (i in 1:length(filenames)){
  testfile <- read.csv(paste0("H:\\CannabisStudy\\CSV\\",files[i]))
  if (length(unique(testfile$SCC.MenuSearch)) == 4 ){
    print(filenames[i])
  }
}

#writing a function for computing standard deviation of lane deviation  for control group

#we will first have to trim data to create the control group
#for file m1 it should look something like controlm1 <- m1[control]
#for (i in 1:length(#all the files)) {
#sd.control <- sd(controlm1$SCC.Spline.Lane.Deviation.2)
#}

#writing a function for computing average speed for control group
#for (i in 1:length(#all the files)){
#mean.speed <- mean(controlm1$VDS.Veh.Speed)
#}

#writing a function for computing standard deviation of deviation from speed limit 
#for (i in 1:length(#all the files)) {
#sd.speedlimit <- sd(controlm1$SCC.Within.Speed.Limit)
#}

