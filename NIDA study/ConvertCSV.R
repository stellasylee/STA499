library ("readxl")
library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)
library(dplyr)

# Upload Data ----

haptic <- read_excel("/Users/seoyeon/Documents/2019MAP/haptic/output_v27.xls")
## V2V Safety Systems Forward Collision Warning event
FCW <- read_excel("/Users/seoyeon/Documents/2019MAP/haptic/FCWCompletes_AllSites_EditThis.xlsx")
## V2V Safety Systems Left Turn and Intersection Movement Assist events --> auditory only IMA events "IM"
IMLT <- read_excel("/Users/seoyeon/Documents/2019MAP/haptic/IMLTCombinedData.xlsx")

# IMA event
# speed limit auditory 45mph vs haptic 35mph
# 4 sites in auditory  haptic is only UofIowa

tempList <- m1$elemDataI
length(tempList)
row <- tempList[[1]]
leng <- length(tempList[[1]])
for (i in 1:leng){
  
}
files <- list.files("/Users/seoyeon/Documents/2019MAP/Reduced")
temp <- readMat(paste0("/Users/seoyeon/Documents/2019MAP/Reduced/",files[1]))
data <- temp$elemData
leng <- length(data[[1]])
tempdf <- data.frame("ID" = rep(7, leng), "Visit" = rep(1, leng))
r <- rownames(data)
lane <- which(rownames(data) == 'SCC.Lane.Deviation')
tlane <- data[lane]
for (var in 1:length(data)){
  varName = paste0(r[var])
  #cbind(tempdf, varName = data[var])
  tempdf[varName] = data[var]
}
convertToCsv <- function (data){
  # find participantID, visit number, Condition
  
  # Create dataframe including participantID, visit
  rowN <- length(data[[1]]) # how many rows this data contain
  
  # Add variables from elemData
  for (var in 1:length(data)){ # for each variables in the elemData
    
  }
}
