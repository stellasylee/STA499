install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lme4")
install.packages("lmerTest")
install.packages("lqmm")

library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
library(plotly)
library(lqmm)

#final Mpdels
#Artist Task

#loading required data
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

#filtering to keep only valid artist from ArtistExperiment
validArtist <- filter(AnalysisArtist, AnalysisArtist$valid > 0)

#filtering to keep only engaged data
keepingengagement <- dplyr::filter(ArtistExperiment, !((valid == 0) & (incorrect == 0)))

#reading in message task files
eventTimesMessage <- read.csv("H:\\CannabisStudy\\message\\eventTimesMessage.csv")
analysisMes <- read.csv("H:\\CannabisStudy\\message\\analysisMesBrake.csv")

#filtering out and creating separate dataframes for experimental and control groups

MessageExperiment <- dplyr::filter(analysisMes, Experiment == "1" )

MessageControl <- dplyr::filter(analysisMes, Experiment == "0")

#sorting these dataframes to ensure that the the rows in both of them align

MessageExperiment <- arrange(MessageExperiment, desc(DaqName, eventNum))
MessageControl <- arrange(MessageControl, desc(DaqName, eventNum))

MessageExperiment <- mutate(MessageExperiment, SD.Lane.Diff = MessageExperiment$SD.Lane.Deviation - MessageControl$SD.Lane.Deviation,
                            Avg.Speed.Diff = MessageExperiment$Avg.Speed - MessageControl$Avg.Speed, 
                            SD.Speed.Diff = MessageExperiment$Sd.Speed - MessageControl$Sd.Speed,
                            Break.Diff = MessageExperiment$Max.Brake - MessageControl$Max.Brake)


##########General performance Models
##artist task

#lane Deviation
fit <- lmer(data = validArtist, log(SD.Lane.Deviation) ~ (1 | ID) + (Experiment) + THC  + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Avg Speed
fit <- lmer(data = validArtist,  Avg.Speed ~ (1 | ID) + Experiment + THC + BAC + experimentallength + factor(pageNum))

#SD Speed
fit <- lmer(data = validArtist,  SD.Speed ~ (1 | ID) + Experiment +  THC + BAC + experimentallength  + (pageNum > 1))

##message task
#lane Deviation
mesfit <- lmer(data = analysisMes, SD.Lane.Deviation ~ (1 | ID) + Experiment + Experiment:BAC + THC + BAC + Avg.Speed + factor(LogStreams.5)) 
summary(mesfit)

#Average speed
mesfit <- lmer(data = analysisMes, Avg.Speed ~ (1 | ID) + Experiment + Experiment:BAC + THC + BAC +  factor(LogStreams.5))

#SD Speed
mesfit <- lmer(data = analysisMes,  Sd.Speed ~ (1 | ID) + Experiment + THC + BAC + factor(LogStreams.5))

#paired Difference models
#artist task

#message task
mesfit <- lmer(data = MessageExperiment, SD.Lane.Diff ~ (1 | ID) + ns(THC, 3) + BAC + Avg.Speed  + factor(LogStreams.5))
summary(mesfit)

#Average Speed
mesfit <- lmer(data = MessageExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5))
summary(mesfit)

#SD Speed
mesfit <- lmer(data = MessageExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5)) 
summary(mesfit)

#artist task paired models
#refiltering valid artist to use Artist Experiment instead of Analysis Artist

validArtist <- filter(ArtistExperiment, ArtistExperiment$valid > 0)

#lane deviation
fit <- lmer(data = validArtist, SD.Lane.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#average speed
fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(pageNum) + experimentallength)
summary(fit)

#SD Speed
fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum) + experimentallength)
summary(fit)
