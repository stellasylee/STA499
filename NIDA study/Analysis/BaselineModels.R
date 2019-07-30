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

#Creating baseline models using only control data

#reading in required data
AnalysisArtist <- read.csv("H:\\CannabisStudy\\artist\\artistTaskAnalysis.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\artist\\artistTimes.csv")

#converting the length of experimental segment into seconds from frames
AnalysisArtist$experimentallength <- (AnalysisArtist$experimentallength /60)

#filtering out and creating separate dataframes for experimental and control groups
ArtistExperiment <- filter(AnalysisArtist, Experiment == "1" )

ArtistControl <- filter(AnalysisArtist, Experiment == "0")

#filtering to keep in only valid artist in artist control
validArtistC <- filter(ArtistControl, ArtistControl$valid > 0)

#keepeing engagement control group
keepingengagementC <- dplyr::filter(ArtistControl, !((valid == 0) & (incorrect == 0)))

#lane Deviation
fit <- lmer(data = keepingengagementC, log(SD.Lane.Deviation) ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)
anova(fit)
AIC(fit)
#Avg Speed
fit <- lmer(data = keepingengagementC,  Avg.Speed ~ (1 | ID) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)
AIC(mesfit)
#SD Speed
fit <- lmer(data = keepingengagementC,  SD.Speed ~ (1 | ID) + THC + BAC + experimentallength  + factor(pageNum))
summary(fit)

#message reading task
#reading in message task files
eventTimesMessage <- read.csv("H:\\CannabisStudy\\message\\eventTimesMessage.csv")
analysisMes <- read.csv("H:\\CannabisStudy\\message\\analysisMesBrake.csv")

#filtering out and creating separate dataframes for experimental and control groups
MessageExperiment <- dplyr::filter(analysisMes, Experiment == "1" )

MessageControl <- dplyr::filter(analysisMes, Experiment == "0")

#lane Deviation
mesfit <- lmer(data = MessageControl, SD.Lane.Deviation ~ (1 | ID) + eventNum  + THC + BAC + Avg.Speed + factor(LogStreams.5)) 
summary(mesfit)
anova(mesfit)

#Average speed
mesfit <- lmer(data = MessageControl, Avg.Speed ~ (1 | ID)  + THC + BAC +  factor(LogStreams.5) + eventNum)
summary(mesfit)
AIC(mesfit)

#SD Speed
mesfit <- lmer(data = MessageControl,  Sd.Speed ~ (1 | ID) + THC + BAC + factor(LogStreams.5) + eventNum)
summary(mesfit)
AIC(mesfit)