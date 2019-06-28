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

# Convert every analyze/reduced files into csv format including participant id and visit number
for (i in 1:length(fileNames)){
  convertToCSV(fileNames[i], as.numeric(substr(disp$DaqPath[i],1,3)),disp$Visit[i])
}

# converToCSV----
## input----
## fileName: name of the file needed to covert to csv
## id: participant id from disposition file
## visit: visit number from disposition file
## output----
## save csv file version of this .mat file in assigned directory
convertToCSV <- function (fileName, id, visit){
  path <- paste0("H:\\NIDA\\Reduced\\",fileName)
  temp <- readMat(path)
  data <- temp$elemData
  # Create dataframe including participantID, visit
  rowN <- length(data[[1]]) # how many rows this data contain
  leng <- length(data[[1]])
  tempdf <- data.frame("DaqName" = rep(fileName, rowN),"ID" = rep(id, rowN), "Visit" = rep(visit, rowN))
  r <- rownames(data)
  # Add variables from elemData
  for (var in 1:length(data)){
    varName = paste0(r[var])
    if (ncol(data[[var]]) == 1){
      tempdf[varName] = data[[var]]
    }else{
      for (i in 1:ncol(data[[var]])){
        newColName = paste0(varName,".",i)
        tempdf[newColName] = data[[var]][,i]
      }
    }
  }
  # Convert dataframe to csv file
  write.csv(tempdf, file = paste0("H:\\NIDA\\ReducedCSV\\",substr(fileName, 1, nchar(fileName)-4), ".csv"))
}
