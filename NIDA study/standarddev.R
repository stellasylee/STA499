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


#testfile for checking how to find standard devation 
tvar <- c("ScenarioOrder", "Subject")
sdtest <- updateddisposition[tvar]
sss <- 5
eee <- 10
rowstouse <- sss:eee
sd(sdtest$ScenarioOrder[rowstouse])

#code to calculate sd would be
#sd(m1$SCC.Lane.Deviation.2[start:end])
#writing this as a function

#sdlanedev <- function(file){
# for (i in length(all the files)) {
#  sd(m1$SCC.Lane.Deviation.2[start:end])

#}
#}

#trying to figure out how you find which rows would comprise the control group if we have the experimental group
#start <- (start of experimental group -(length(end experimental - start experimental)))
#end <- (start of the experimental group)