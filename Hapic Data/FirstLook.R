# First look at time-series data
# This code will read a single .mat file into R
# Then we will look at the logstream variables to locate an event of interest

#install.packages("R.matlab")
library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)
# NOTE: should have the folder "MatFiles" downloaded and extracted to the location below
# Should have the files "disposition.xls" and "randomization.xlsx" in locations below

## Grab all of the .mat Files from directory H:\V2VMAT\MatFiles
files <- list.files("H:\\V2VMAT\\MatFiles")

## Read in the first one (will automate later)
m1 <- readMat(paste0("H:\\V2VMAT\\MatFiles\\",files[1]))

#### Who's in this file?
## first load disposition file
disp <- readxl::read_excel("H:\\disposition.xls")

## locate this .mat file
date.strs <- str_replace(disp$DaqName, ".daq", ".mat")
which(date.strs == files[1]) # This is record number 225 is the disposition file

disp[225,]  # Record 225's data was Reduced, so it was analyzed in the study

# Their participant ID is 0074 and they are a in the Y age group and M gender

## Link with randomization info
rand <- readxl::read_excel("H:\\MainTestMatrix.xlsx")
print(rand[which(rand$`Participant#` == '0074'),])

# We see that this participant received experimental condition IPI2
# This is "Inter-pulse interval Alert level 2 (see table 1 in Haptic project descript)



### What can we link with the scenario description?

## Find position of log-stream vars in list 
ls.pos <- which(rownames(m1$elemDataI) == 'SCC.LogStreams')

## Distrib of first log-stream - counter of events from start to finish
table(m1$elemDataI[[ls.pos]][,1])

## Distrib of second log-stream - ID of the active event
table(m1$elemDataI[[ls.pos]][,2])

## log-streams 3:5 are all empty here 
#table(m1$elemDataI[[ls.pos]][,5])

## Find pos of haptic alert in list
hap.pos <- which(rownames(m1$elemDataI) == 'SCC.Haptic.Alert')

## pos of time in list
time.pos <- which(rownames(m1$elemDataI) == 'Time')

## Plot log-streams and haptic alert vs. time
plot(m1$elemDataI[[time.pos]], m1$elemDataI[[ls.pos]][,1], type = "l")
lines(m1$elemDataI[[time.pos]], m1$elemDataI[[ls.pos]][,2], col = "blue")
lines(m1$elemDataI[[time.pos]], m1$elemDataI[[hap.pos]], col = "red")

### Interpretation?? (trying to mesh w/ the haptic project report Sec 2.2.2)
# Log-stream 1 == 1 -> Alert begins 1.06 seconds prior to incursion vehicle becoming visible
# Log-stream 1 == 2 & Log-stream 2 == 2 -> Incursion vehicle is visible
# Log-stream 1 == 3 -> Collision occurs 2.90 seconds after incursion vehicle becomes visible

## Driver performance during time when incursion veh is visisble?
ls1id2 <- which(m1$elemDataI[[ls.pos]][,1] == 2)

## Braking?
brk.pos <- which(rownames(m1$elemDataI) == 'CFS.Brake.Pedal.Force')
plot(m1$elemDataI[[time.pos]][ls1id2], m1$elemDataI[[brk.pos]][ls1id2], type = "l")

# This person clearly applied the breaks shortly after the vehicle became visible

## Steering wheel angle rate
steerrate.pos <- which(rownames(m1$elemDataI) == 'CFS.Steering.Wheel.Angle.Rate')

plot(m1$elemDataI[[time.pos]], m1$elemDataI[[steerrate.pos]], type = "l", main = "Steering Rate - Zoomed Out")
abline(v = m1$elemDataI[[time.pos]][min(ls1id2)], col = "red", lty = 2)
abline(v = m1$elemDataI[[time.pos]][max(ls1id2)], col = "red", lty = 2)

plot(m1$elemDataI[[time.pos]][ls1id2], m1$elemDataI[[steerrate.pos]][ls1id2], type = "l", main = "Steeting Rate - Zoomed In")
abline(v = m1$elemDataI[[time.pos]][min(ls1id2)], col = "red", lty = 2)
abline(v = m1$elemDataI[[time.pos]][max(ls1id2)], col = "red", lty = 2)

# It appears this person swerved, see Table 2 in report which documents that
# time until turning wheel 25 deg or greater was used as a dependent variable

## Find first response ----
# when was haptic alert? (get row when haptic alert column changed to 9)
haptic <- which(rownames(m1$elemDataI) == 'SCC.Haptic.Alert')
table(m1$elemDataI[[haptic]][,1])
h <- m1$elemDataI[[haptic]][,1]
alert <- -1
for (i in 1: 13349){
  if (h[i] != 0){
    alert <- i
    break
  }
}

# accelerator pedal release
# accelerator pedal press
# brake
brk.pos <- which(rownames(m1$elemDataI) == 'CFS.Brake.Pedal.Force')
plot(m1$elemDataI[[time.pos]][ls1id2], m1$elemDataI[[brk.pos]][ls1id2], type = "l")

# steer
