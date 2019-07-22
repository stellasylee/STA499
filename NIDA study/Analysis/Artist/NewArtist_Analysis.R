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
fit <- lmer(data = ArtistExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(pageNum) + experimentallength)
summary(fit)

#SD Speed
fit <- lmer(data = ArtistExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum) + experimentallength)
summary(fit)

#modelling the data with quantile regression with mixed effects

#lane deviation
mesfit <- lqmm(SD.Lane.Diff ~ THC + BAC + factor(pageNum) + Avg.Speed, random = ~ 1, group = ID, data = ArtistExperiment,
               tau = 0.75)
summary(mesfit)

#Avg.Speed
mesfit <- lqmm(Avg.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~ 1, group = ID, data = ArtistExperiment,
               tau = 0.75)
summary(mesfit)

#SD.Speed
mesfit <- lqmm(SD.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~ 1, group = ID, data = ArtistExperiment,
               tau = 0.75)
summary(mesfit)


#filtering to keep only valid artist from ArtistExperiment

validArtist <- filter(ArtistExperiment, ArtistExperiment$valid > 0)

#modelling with median as well
#Lane deviation with median
fit <- lqmm(SD.Lane.Diff ~ THC + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#Lane deviation with two THC groups with median

fit <- lqmm(SD.Lane.Diff ~ (THC == 0) + THC + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)

summary(fit)

#Lane Deviation with two THC groups with mean

fit <- lmer(data = validArtist, SD.Lane.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Lane deviation with log(THC + 1) with mean
fit <- lmer(data = validArtist, SD.Lane.Diff ~ (1 | ID) + log(THC + 1) + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Lane deviation with log(THC + 1 with median)
fit <- lqmm(SD.Lane.Diff ~ log(THC + 1) + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)


#Average speed

#average speed with median

fit <- lqmm(Avg.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with median

fit <- lqmm(Avg.Speed.Diff ~ (THC == 0) + THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#THC significant with positive coeffecient

#two THC groups with mean

fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)

#Log(THC + 1 with mean)
fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + log(THC + 1) + BAC + experimentallength + factor(pageNum))

summary(fit)

##Log(THC + 1 with median)

fit <- lqmm(Avg.Speed.Diff ~ log(THC + 1) + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#SD Speed 

#SD speed with median

fit <- lqmm(SD.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with median
fit <- lqmm(SD.Speed.Diff ~ (THC == 0) + THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with mean

fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)

#Log(THC + 1 with mean)
fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + log(THC + 1) + BAC + experimentallength + factor(pageNum))

summary(fit)

##Log(THC + 1 with median)

fit <- lqmm(SD.Speed.Diff ~ log(THC + 1) + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)
