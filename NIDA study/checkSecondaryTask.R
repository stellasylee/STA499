library(stringr) # String manipulation
library(tidyverse)
library ("readxl")
library ("dplyr")

files <- list.files("H:\\NIDA\\ReducedCSV")
disp <- readxl::read_excel("H:\\NIDA\\disposition.xls")
# filter ignore/discard data
disp <- filter (disp, is.na(Ignore)) %>% filter(., is.na(Discard))

dispRNA <- filter(disp, Restart =='RNA')
fileNames <- str_replace(dispRNA$DaqName, ".daq", ".csv")
for (i in 1:length(fileNames)){
  tempFile <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  if (length(unique(tempFile$SCC.MenuSearch)) != 4){
    print(fileNames[i])
  }
}


tempFile <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", "20130206102259.csv"))
if (length(unique(tempFile$SCC.MenuSearch)) != 4)
  print(files[i])
length(unique(tempFile$SCC.MenuSearch))
unique(tempFile$SCC.MenuSearch)
