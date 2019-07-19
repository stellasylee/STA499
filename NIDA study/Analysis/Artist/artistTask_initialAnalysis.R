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
library(ggplot2)
#reading in files

AnalysisArtist <- read.csv("H:\\CannabisStudy\\artist\\artistTaskAnalysis.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\artist\\artistTimes.csv")

#converting the length of experimental segment into seconds from frames
AnalysisArtist$experimentallength <- (AnalysisArtist$experimentallength /60)


#checking if THC + BAC groups got more incorrect 
#### fit <- glmer((valid == 0) ~ BAC + THC + factor(pageNum)  + (1 | ID), data = data2, family = "binomial")

fit1 <- glm(data = AnalysisArtist, incorrect ~ THC, family = "binomial")
summary(fit1)

fit2 <- glm(data = AnalysisArtist, valid ~ THC, family = "binomial")
summary(fit2)

#filtering to keep only valid artist ###since they might not be engaged
validArtist <- filter(AnalysisArtist, AnalysisArtist$valid > 0)

View(validArtist)
#filtering Analysis Artist to not include instances when they were not engaged i.e valid = and incorrect = 0

keepingengagement <- filter (AnalysisArtist, !((valid == 0) & (incorrect == 0)))

#modelling the data with keeping artist

fit <- lmer(data = keepingengagement, log (SD.Lane.Deviation) ~ (1 | ID) + Experiment + DosingLevel + Avg.Speed + pageNum)
anova(fit)
#with this model Dosing level YM is a significant predictor

#modelling keepingengagment data with avg speed as a predictor with Dosing Level
fit <- lmer(data = keepingengagement, Avg.Speed ~ (1 | ID) + Experiment + DosingLevel + pageNum)
anova(fit)

#modelling keepingengagment data with avg speed as a predictor with THC and BAC
fit <- lmer(data = keepingengagement, Avg.Speed ~ (1 | ID) + Experiment + THC + BAC +  pageNum)
summary(fit)
#THC and BAC are not good predictors here

#modelling keepingengagment data with SD speed as a predictor with Dosing Level
fit <- lmer(data = keepingengagement, SD.Speed ~ (1 | ID) + Experiment + DosingLevel + pageNum)
summary(fit)
#does not really predict anything

#modelling keepingengagment data with Sd Speedas a predictor with THC and BAC
fit <- lmer(data = keepingengagement, SD.Speed ~ (1 | ID) + Experiment + THC + BAC + pageNum)
summary(fit)
#this model does not predict much either


#modelling the data with filtered data to keep only valid artist
fit <- lmer(data = validArtist, log(SD.Lane.Deviation) ~ (1 | ID) + Experiment + (THC == 0) + THC  + BAC + Avg.Speed + (pageNum > 1))
summary(fit)

ggplot(validArtist, aes(x = log(THC+1), y = SD.Lane.Deviation, color = factor(Experiment))) + geom_point() + geom_smooth(method = "loess")

#checking residual plots of this model

plot(validArtist$THC, resid(fit),    ylab="Residuals", xlab="THC", main="Valid Artist Task") + abline(0,0)
plot(validArtist$ID, resid(fit),    ylab="Residuals", xlab="ID", main="Valid Artist Task") + abline(0,0)
plot(validArtist$Experiment, resid(fit),    ylab="Residuals", xlab="Experiment", main="Valid Artist Task") + abline(0,0)
plot(validArtist$BAC, resid(fit),    ylab="Residuals", xlab="BAC", main="Valid Artist Task") + abline(0,0)
plot(validArtist$Avg.Speed, resid(fit),    ylab="Residuals", xlab="Avg.Speed", main="Valid Artist Task") + abline(0,0)
plot(validArtist$pageNum, resid(fit),    ylab="Residuals", xlab="pageNum", main="Valid Artist Task") + abline(0,0)

#exploring other models
fit <- lmer(data = validArtist,  Avg.Speed ~ (1 | ID) + Experiment + THC + BAC + experimentallength + (pageNum > 1))
summary(fit)


fit <- lmer(data = validArtist,  SD.Speed ~ (1 | ID) + Experiment +  THC + BAC + experimentallength  + (pageNum > 1) )
summary(fit)

head(validArtist)

