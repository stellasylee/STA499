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

# Test with one/two file----
m1 <- readMat(paste0("H:\\NIDA\\MatFiles\\",files[1]))
m2 <- readMat(paste0("H:\\NIDA\\MatFiles\\",files[2]))
which(disp$DaqName == files[1]) # 59 in disp
# Detect secondary tasks from file
## menu task time
### starting times 
menuTime <- which(rownames(m1$elemDataI) == 'SCC.MenuSearch')
table(m1$elemDataI[[menuTime]][,1])
plot (m1$elemDataI[[menuTime]][,1])
menuS <- m1$elemDataI[[menuTime]][,1]

#match(1,menuS) --> when event 1 available
#[1] 35064 
#match(1,menuF) --> when completed the task
#[1] 35395
### ending times
menuScore <- which(rownames(m1$elemDataI) == 'AUX1.MenuScore')
table(m1$elemDataI[[menuScore]][,1])
plot (m1$elemDataI[[menuScore]][,1])
menuF <- m1$elemDataI[[menuScore]][,1]

mF <- m2$elemDataI[[which(rownames(m2$elemDataI) == 'AUX1.MenuScore')]][,1]
table(mF)
plot(mF)
