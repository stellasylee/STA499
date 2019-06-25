library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)
library ("readxl")
library ("dplyr")
library(doParallel)
library(foreach)

# Upload files ----
files <- list.files("H:\\NIDA\\MatFiles") # list of name of .mat files
disp <- readxl::read_excel("H:\\NIDA\\disposition.xls")
# filter ignore/discard data
disp <- filter (disp, is.na(Ignore)) %>% filter(., is.na(Discard))
fileNames <- str_replace(disp$DaqName, ".daq", ".mat")

#registerDoParallel(cores=detectCores()-1)
registerDoParallel(cores=6)
foreach (i=1:1) %dopar% {
  temp <- R.matlab::readMat(paste0("H:\\NIDA\\MatFiles\\",files[i]))
  temp <- temp$elemDataI
  # Detect secondary task times
  # Extract columns needed for analysis
  # Filter necessary timeframe
  
}
system.time(foreach (i=1:1) %dopar% {
  original <- R.matlab::readMat(paste0("H:\\NIDA\\MatFiles\\",files[i]))
})
