library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)
library ("readxl")
library ("dplyr")

# Upload files ----
files <- list.files("H:\\NIDA\\Reduced") # list of name of .mat files
disp <- readxl::read_excel("H:\\NIDA\\disposition.xls")
# filter ignore/discard data
disp <- filter (disp, is.na(Ignore)) %>% filter(., is.na(Discard))
fileNames <- str_replace(disp$DaqName, ".daq", ".mat")
path <- paste0("H:\\NIDA\\Reduced\\",files[1])
data <- readMat(path)

write.csv(df, file = paste0(path.substring(0, path.length() - 4)), ".csv")
