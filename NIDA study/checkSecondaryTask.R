library(stringr) # String manipulation
library(tidyverse)
library ("readxl")
library ("dplyr")

files <- list.files("H:\\NIDA\\ReducedCSV")
disp <- readxl::read_excel("H:\\NIDA\\disposition.xls")
# filter ignore/discard data
disp <- filter (disp, is.na(Ignore)) %>% filter(., is.na(Discard))


fileNames <- str_replace(disp$DaqName, ".daq", ".csv")
noevents <- c()
for (i in 1:length(fileNames)){
  tempFile <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  if ((length(unique(tempFile$SCC.MenuSearch)) != 4)&&(length(unique(tempFile$SCC.MenuSearch)) != 5)){
    noevents <- append (noevents, fileNames[i])
  }
}

disp$DaqName <- str_replace(disp$DaqName, ".daq", ".csv")
filteredDisp <- filter (disp, !(DaqName %in% noevents))
# drop red case
