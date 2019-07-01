library(stringr)
library(tidyverse)
library ("dplyr")
library(lme4)

analysisfile <- read.csv("H:\\NIDA\\analysisMatrix.csv")
analysisfile <- analysisfile[order(analysisfile$ID, analysisfile$Experiment),]

fit1 <- lmer(SD.Lane.Deviation ~ Experiment + factor(DosingLevel) + (1 | ID), data = analysisfile)
summary(fit1)

fit3 <- lmer(SD.Lane.Deviation ~ Experiment + factor(DosingLevel) + Experiment:factor(DosingLevel) + (1 | ID), data = analysisfile)
