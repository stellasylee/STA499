install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("foreach")
install.packages("lme4")
install.packages("lmerTest")
install.packages("lqmm")

library(MASS)
library(lqmm)
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
#reading in files

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


#Modelling the data with Message Experiment

#lane deviation

mesfit <- lmer(data = MessageExperiment, SD.Lane.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(LogStreams.5))
summary(mesfit)

### BAC is significant predictor of lane deviation####

#Average Speed
mesfit <- lmer(data = MessageExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5)) 
summary(mesfit)

#### THC significant predictor with positive coefficient####

#standard deviation speed
mesfit <- lmer(data = MessageExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5)) 
summary(mesfit)
AIC(mesfit)

#max brake force

mesfit <- lmer(data = MessageExperiment, Break.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5) + Avg.Speed) 
summary(mesfit)

#running mixed effects quantile regression
#lane deviation
mesfit <- lqmm(SD.Lane.Diff ~ THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.75)
summary(mesfit)
###### BAC significant predictor lane deviation#####

#Avg Speed
mesfit <- lqmm(Avg.Speed.Diff ~ THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.75)
summary(mesfit)
#### THC significant predictor with positive coefficient####

#SD speed
mesfit <- lqmm(SD.Speed.Diff ~ THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.75)
summary(mesfit)


#max brake force
mesfit <- lqmm(Break.Diff ~ THC + BAC + Avg.Speed, random = ~ 1, group = ID, data = MessageExperiment,
                      tau = 0.75)
summary(mesfit)               


#using median for quantile regression  
#lane deviation
mesfit <- lqmm(SD.Lane.Diff ~ THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)
###### BAC significant predictor lane deviation#####

#Avg Speed
mesfit <- lqmm(Avg.Speed.Diff ~ THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)
#### THC significant predictor with positive coefficient####

#SD speed
mesfit <- lqmm(SD.Speed.Diff ~ THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)


#max brake force
mesfit <- lqmm(Break.Diff ~ THC + BAC + Avg.Speed, random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)              



#log transformed values of THC
#lane deviation
mesfit <- lqmm(SD.Lane.Diff ~ log(THC + 1) + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)
###### BAC significant predictor lane deviation#####

#Avg Speed
mesfit <- lqmm(Avg.Speed.Diff ~ log(THC + 1) + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)

#SD speed
mesfit <- lqmm(SD.Speed.Diff ~ log(THC + 1) + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)


#max brake force
mesfit <- lqmm(Break.Diff ~ log(THC + 1) + BAC + Avg.Speed, random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)              


#using multiple THC groups (THC = 0 + THC)

#lane deviation
mesfit <- lqmm(SD.Lane.Diff ~ (THC == 0) + THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)
###### BAC significant predictor lane deviation#####

#Avg Speed
mesfit <- lqmm(Avg.Speed.Diff ~ (THC == 0) + THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)
####### THC significant with positive coefficient#########

#SD speed
mesfit <- lqmm(SD.Speed.Diff ~ (THC == 0) + THC + BAC + Avg.Speed + factor(LogStreams.5), random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)


#max brake force
mesfit <- lqmm(Break.Diff ~ (THC == 0) + THC + BAC + Avg.Speed, random = ~ 1, group = ID, data = MessageExperiment,
               tau = 0.5)
summary(mesfit)       

#creating a column in message experiment to factor road segment
library(plyr)
MessageExperiment <- mutate(MessageExperiment, roadsegment = LogStreams.5)
MessageExperiment$roadsegment <- as.factor(MessageExperiment$roadsegment)

#renaming the road segment column
MessageExperiment$roadsegment <- revalue(MessageExperiment$roadsegment, 
    c("11" = "urban","12" = "urban", "13" = "urban", "22" = "interstate", "32" = "rural", "33" = "urban"))

library(ggplot2)
#looking at the plots
ggplot(data = MessageExperiment, aes (x = THC, y = SD.Lane.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~roadsegment , scales = 'free') + geom_smooth()

#filtering to keep only interstate
interstatemes <- dplyr::filter(MessageExperiment, roadsegment == "interstate")

#filtering to keep only urban
urbanmes <- dplyr::filter(MessageExperiment, roadsegment == "urban")

#filtering to keep only interstate
ruralmes <- dplyr::filter(MessageExperiment, roadsegment == "rural")

#Paired lane deviation differnces by participants and road segments
ggplot(data = interstatemes, aes (x = THC, y = SD.Lane.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()

ggplot(data = urbanmes, aes (x = THC, y = SD.Lane.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()

ggplot(data = ruralmes, aes (x = THC, y = SD.Lane.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()

#Paired average speed differnces by participants and road segments

ggplot(data = interstatemes, aes (x = THC, y = Avg.Speed.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()

ggplot(data = urbanmes, aes (x = THC, y = Avg.Speed.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()

ggplot(data = ruralmes, aes (x = THC, y = Avg.Speed.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()
