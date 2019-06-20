#install.packages("R.matlab")
library(R.matlab) # Reads matlab files
library(stringr) # String manipulation
library(tidyverse)

## Grab all of the .mat Files from directory H:\V2VMAT\MatFiles
files <- list.files("H:\\V2VMAT\\MatFiles")
disp <- readxl::read_excel("H:\\disposition.xls")
rand <- readxl::read_excel("H:\\MainTestMatrix.xlsx")

## Filter 40Hz Intetensity Level Participants
# Get participant number of 40Hz
group40 <- as.vector(rand[which(rand$`AlertLevel` == 2),4])

## Get file names corresponding to participant number

# file40

## For each file (or participant), detect first response
