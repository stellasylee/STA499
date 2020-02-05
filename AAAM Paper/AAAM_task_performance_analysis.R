#####################################################################
#####################################################################
#####                 TASK PERFORMANCE ANALYSES
#####
#####################################################################
#####################################################################


library(dplyr)
library(lme4)
library(optimx)
library(lmerTest)
library(lmtest)
library(knitr)

####################################################
############### Artist #############################
####################################################
ld <- read.csv("H:\\NIDA\\final\\finalFinalArtistLD.csv")


## Only use cases who engaged, only experimental segments
ld2 <- dplyr::filter(ld, valid == 1 | incorrect == 1)
ld2$ID <- as.factor(ld2$ID)
ld2 <- dplyr::filter(ld2, Experiment == 1)

## Task Time (amongst engaged)
m <- lmer(total ~ THC+ BAC + factor(pageNum) + (1 |ID), data = ld2, REML = FALSE, control = lmerControl(
            optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)

## Task time (amongst completed)
ld3 <- dplyr::filter(ld2, valid == 1)
m <- lmer(total ~ THC+ BAC + factor(pageNum) + (1 |ID), data = ld3, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)

## Providing an incorrect response
ld4 <- dplyr::filter(ld, Experiment == 1 & (valid == 1 | incorrect == 1))
fit1 <- glmer(data = ld4, incorrect ~ THC + BAC  + (1|ID), family = "binomial")
summary(fit1)

## Completing the task eventually
fit2 <- glmer(data = ld2, valid ~ THC + BAC + (1|ID) + (pageNum == 1), family = "binomial")
summary(fit2)
AIC(fit2) #  386.4043
exp(coef(fit2))
coefs <- summary(fit2)$coefficients[,1]
ses <- summary(fit2)$coefficients[,2]
kable(round(exp(data.frame(OR = coefs, LCL = coefs - 1.96*ses, UCL = coefs + 1.96*ses)), 4))

####################################################
############### Mirror #############################
####################################################
sm <- read.csv("H:\\NIDA\\final\\finalFinalMirror.csv")


## Task Time (amongst all)
sm2 <- dplyr::filter(sm, Experiment == 1)

m <- lmer(total ~ THC+ BAC + factor( LogStreams.5) + (1 |ID), data = sm2, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)

## Task Time (amongst completed)
sm3 <- dplyr::filter(sm2, valid == 1)

m <- lmer(total ~ THC+ BAC + factor( LogStreams.5) + (1 |ID), data = sm3, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)

