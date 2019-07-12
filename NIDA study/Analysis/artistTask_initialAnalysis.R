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

AnalysisArtist <- read.csv("H:\\CannabisStudy\\validAnalysisArtist.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\validArtistTimes.csv")

View(AnalysisArtist)

#filtering to keep only valid artist


validArtist <- filter(AnalysisArtist, AnalysisArtist$valid > 0)

View(validArtist)


#modelling the data 

fit <- lmer(data = validArtist, log (SD.Lane.Deviation) ~ (1 | ID) + Experiment + THC + BAC + Avg.Speed + factor (pageNum), control = lmerControl(optCtrl = list(maxiter = 30000)) )


#checking residual plots of this model

plot(validArtist$THC, resid(fit),    ylab="Residuals", xlab="THC", main="Valid Artist Task") + abline(0,0)
plot(validArtist$ID, resid(fit),    ylab="Residuals", xlab="ID", main="Valid Artist Task") + abline(0,0)
plot(validArtist$Experiment, resid(fit),    ylab="Residuals", xlab="Experiment", main="Valid Artist Task") + abline(0,0)
plot(validArtist$BAC, resid(fit),    ylab="Residuals", xlab="BAC", main="Valid Artist Task") + abline(0,0)
plot(validArtist$Avg.Speed, resid(fit),    ylab="Residuals", xlab="Avg.Speed", main="Valid Artist Task") + abline(0,0)
plot(validArtist$pageNum, resid(fit),    ylab="Residuals", xlab="pageNum", main="Valid Artist Task") + abline(0,0)


qqnorm(residuals(fit))

#exploring other models

fit <- lmer(data = validArtist,  Avg.Speed ~ (1 | ID) + Experiment + THC + BAC )
summary(fit)

fit <- lmer(data = validArtist,  SD.Speed ~ (1 | ID) + Experiment + THC + BAC )
summary(fit)


