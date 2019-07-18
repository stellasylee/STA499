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
#reading in files

AnalysisArtist <- read.csv("H:\\CannabisStudy\\artist\\artistTaskAnalysis.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\artist\\artistTimes.csv")

#converting the length of experimental segment into seconds from frames
AnalysisArtist$experimentallength <- (AnalysisArtist$experimentallength /60)

#filtering out and creating separate dataframes for experimental and control groups

ArtistExperiment <- filter(AnalysisArtist, Experiment == "1" )

ArtistControl <- filter(AnalysisArtist, Experiment == "0")

#sorting these dataframes to ensure that the the rows in both of them align

ArtistExperiment <- arrange(ArtistExperiment, desc(DaqName, eventNum))
ArtistControl <- arrange(ArtistControl, desc(DaqName, eventNum))

ArtistExperiment <- mutate(ArtistExperiment, SD.Lane.Diff = ArtistExperiment$SD.Lane.Deviation - ArtistControl$SD.Lane.Deviation,
                         Avg.Speed.Diff = ArtistExperiment$Avg.Speed - ArtistControl$Avg.Speed, 
                         SD.Speed.Diff = ArtistExperiment$SD.Speed - ArtistControl$SD.Speed)

#modelling the data with ArtistExperiment

#lane deviation
fit <- lmer(data = ArtistExperiment, SD.Lane.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#average speed
fit <- lmer(data = ArtistExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(pageNum))
summary(fit)

#SD Speed
fit <- lmer(data = ArtistExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)
