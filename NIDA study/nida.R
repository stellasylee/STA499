library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)
library ("readxl")
library ("dplyr")

# Upload files ----
files <- list.files("H:\\NIDA\\MatFiles") # list of name of .mat files
disp <- readxl::read_excel("H:\\NIDA\\disposition.xls")
# filter ignore/discard data
disp <- filter (disp, is.na(Ignore)) %>% filter(., is.na(Discard))
disp$DaqName <- str_replace(disp$DaqName, ".daq", ".mat")

# DosingLevel
