library(stringr)
library(tidyverse)
library ("dplyr")
library(lme4)

analysisfile <- read.csv("H:\\NIDA\\analysisMatrix.csv")
analysisfile <- read.csv("H:\\NIDA\\analysisWithoutEngagementPoint.csv")
analysisfile <- analysisfile[order(analysisfile$ID, analysisfile$Experiment),]

fit1 <- lmer(SD.Lane.Deviation ~ Experiment + factor(DosingLevel) + (1 | ID), data = analysisfile)
summary(fit1)

analysisfile$Experiment <- as.factor(analysisfile$Experiment)
analysisfile$DosingLevel <- as.factor(analysisfile$DosingLevel)
fit3 <- lmer(SD.Lane.Deviation ~ Experiment + DosingLevel + Experiment:DosingLevel + (1 | ID), data = analysisfile)
summary(fit3)

coef(fit3)
plot(analysisfile$SD.Lane.Deviation)
