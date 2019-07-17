#combining AnalysisArtist and ArtistTimes

install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lme4")
install.packages("lmerTest")

library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
library(plotly)
library(tidyverse)
#reading in file
AnalysisArtist <- read.csv("H:\\CannabisStudy\\artist\\analysisArtist.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\artist\\artistTimes.csv")

#sorting both the dataframes to ensure they are in the same order to append a column
SortedArtistTimes <- arrange(ArtistTimes, desc(DaqName, eventNum))

sortedAnalysisArtist <- arrange(AnalysisArtist, desc(DaqName, eventNum))

rm(sortedMatrixArtist)

View(SortedArtistTimes)
View(sortedAnalysisArtist)

#adding the event length column to sorted Analysis Artist
#length of experimental group = end-engage

SortedArtistTimes <- mutate(SortedArtistTimes, experimentallength = (end - engagement))  

#duplicating each row so that sortedartisttimes matches sorted sortedanalysisartist

SortedArtistTimes <- SortedArtistTimes[rep(1:nrow(SortedArtistTimes),each=2),] 

#removing the blank rows from sortedanalysisartist

sortedAnalysisArtist <- slice(sortedAnalysisArtist, 1:680)

#adding experimental length column to sorted analysis artist
sortedAnalysisArtist <- add_column(sortedAnalysisArtist, SortedArtistTimes$experimentallength)

view(sortedAnalysisArtist)

write.csv(sortedAnalysisArtist, "H:\\CannabisStudy\\artist\\artistTaskAnalysis.csv")
 