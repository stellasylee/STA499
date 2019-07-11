install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("foreach")
install.packages("lme4")

library(rio)
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
#reading in files

AnalysisArtist <- read.csv("H:\\CannabisStudy\\validAnalysisArtist.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\validArtistTimes.csv")

View(AnalysisArtist)

#filtering to keep only valid artist


validArtist <- filter(AnalysisArtist, AnalysisArtist$valid > 0)


#modelling the data 

fit <- lmer(data = validArtist, log (SD.Lane.Deviation) ~ (1 | ID) + Experiment + THC + BAC + Avg.Speed + pageNum )
summary(fit)

fit <- lmer(data = validArtist,  Avg.Speed ~ (1 | ID) + Experiment + THC + BAC )
summary(fit)

fit <- lmer(data = validArtist,  SD.Speed ~ (1 | ID) + Experiment + THC + BAC )
summary(fit)



