install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("foreach")
install.packages("lme4")
install.packages("lmerTest")

library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
#reading in files

eventTimesMessage <- read.csv("H:\\CannabisStudy\\message\\eventTimesMessage.csv")
analysisMes <- read.csv("H:\\CannabisStudy\\message\\analysisMes.csv")

View(eventTimesMessage)
View(analysisMes)


summary(mesfit)
#fitting a model
#Model 1: SD lane dev as outcome and dosing level as predictor

mesfit <- lmer(data = analysisMes, SD.Lane.Deviation ~ (1 | ID) + Experiment + DosingLevel + Avg.Speed + eventNum)
summary(mesfit)

#Model 2: using message instead of eventNum

mesfit <- lmer(data = analysisMes, SD.Lane.Deviation ~ (1 | ID) + Experiment + DosingLevel + Avg.Speed + message)
summary(mesfit)

#Model 3: using THC and BAC levels instead of Dosing Levels

mesfit <- lmer(data = analysisMes, SD.Lane.Deviation ~ (1 | ID) + Experiment + THC + BAC + Avg.Speed + eventNum)
summary(mesfit)
#experiment Avg. Speed and EventNum are significant predictors of SD lane dev


#Model 4: using Avg Speed as an outcome

mesfit <- lmer(data = analysisMes, Avg.Speed ~ (1 | ID) + Experiment + THC + BAC + eventNum)
summary(mesfit)           
#THC and eventNum are significant predictors of Avg Speed in this model

#Model 5:
mesfit <- lmer(data = analysisMes, Avg.Speed ~ (1 | ID) + Experiment + THC + BAC + message)
summary(mesfit)  
#THC is an extremely good predictor of avg speed in this model

#Model 6: using sp.speed as an outcome
mesfit <- lmer(data = analysisMes,  Sd.Speed ~ (1 | ID) + Experiment + THC + BAC + eventNum)
summary(mesfit)             
