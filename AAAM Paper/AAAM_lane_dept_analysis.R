#####################################################################
#####################################################################
#####                 LANE DEPARTURE ANALYSES
#####
#####################################################################
#####################################################################
library(dplyr)
library(lme4)
library(optimx)


####################################################
############### Artist #############################
####################################################
ld <- read.csv("H:\\NIDA\\final\\finalFinalArtistLD.csv")


## Only use cases who engaged
ld <- dplyr::filter(ld, valid == 1 | incorrect == 1)
ld$ID <- as.factor(ld$ID)

## Binary outcomes
ld$any_dept <- ifelse(ld$Lane.Departure.Minor > 0, 1, 0)
ld$major_dept <- ifelse(ld$Lane.Departure.Major > 0, 1, 0)
ld$sev_dept <- ifelse(ld$Lane.Departure.Severe > 0, 1, 0)

## Only use task period
ld2 <- dplyr::filter(ld, Experiment == 1)

## Create absolute initial position as a covariate
ld2$abs_pos <- abs(ld2$Init.Lane.Pos)

### Unit conversions
ld2$BAC <- ld2$BAC*100


#######################################################
########## PART 1 --- Existence of Departures #########
#######################################################


## Any dept
#res <- glmer(any_dept ~ THC  + BAC  + Avg.Speed + abs_pos + (1 | ID) , data = ld2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
res <- glm(any_dept ~ THC  + BAC + Avg.Speed + abs_pos + SD.Speed , data = ld2, family = "binomial")
summary(res)
AIC(res) ## AIC = 256.2 w/o interaction, 257.3 w/ interaction
exp(res$coefficients)
exp(confint(res, method = "Wald"))

## Major dept
#res <- glmer(major_dept ~ THC  + BAC + Avg.Speed + abs_pos + SD.Speed  + (1 | ID) , data = ld2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
res <- glm(major_dept ~ THC  + BAC + THC:BAC + Avg.Speed + abs_pos + SD.Speed , data = ld2, family = "binomial")
summary(res)
AIC(res) ## AIC 178.0 w/o int, 179.9 w/ int
exp(res$coefficients)
exp(confint(res, method = "Wald"))



## Severe dept
res <- glmer(sev_dept ~ THC  + BAC   +  abs_pos  + (1 | ID) , data = ld2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(res)
AIC(res) ## AIC 37.8 w/o int, 39.6 w/ int
exp(coef(res)$`ID`[1,])
exp(confint(res, method = "Wald"))




#######################################################
########## PART 2 --- Duration of Departures ##########
#######################################################


### Any
ld3 <- dplyr::filter(ld2, Lane.Departure.Minor > 0)
#m <- lmer(Lane.Departure.Minor ~ THC+ BAC + scale(Avg.Speed) + abs_pos  + (1 |ID), data = ld3, REML = FALSE, control = lmerControl(
#            optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m <- lm(Lane.Departure.Minor ~ THC+ BAC + scale(Avg.Speed) + abs_pos + SD.Speed, data = ld3)
summary(m)
confint(m, method = "Wald")


### Major
ld3 <- dplyr::filter(ld2, Lane.Departure.Major > 0)
#m <- lmer(Lane.Departure.Major ~ THC+ BAC + scale(Avg.Speed) + abs_pos + SD.Speed + (1 |ID), data = ld3, REML = FALSE, control = lmerControl(
#            optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m <- lm(Lane.Departure.Major ~ THC+ BAC + scale(Avg.Speed) + abs_pos + SD.Speed, data = ld3)
summary(m)
confint(m, method = "Wald")

### Severe
ld3 <- dplyr::filter(ld2, Lane.Departure.Severe > 0)
#m <- lmer(Lane.Departure.Severe ~ THC+ BAC + scale(Avg.Speed) + abs_pos + SD.Speed + (1 |ID), data = ld3, REML = FALSE, 
#                                                                          control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m <- lm(Lane.Departure.Severe ~ THC+ BAC + scale(Avg.Speed) + abs_pos, data = ld3)
summary(m)


####################################################
############### Mirror #############################
####################################################
sm <- read.csv("H:\\NIDA\\final\\finalFinalMirror.csv")

## Binary outcomes
sm$any_dept <- ifelse(sm$Lane.Departure.Minor > 0, 1, 0)
sm$major_dept <- ifelse(sm$Lane.Departure.Major > 0, 1, 0)
sm$sev_dept <- ifelse(sm$Lane.Departure.Severe > 0, 1, 0)

## Exp period only, add initial position
sm2 <- dplyr::filter(sm, Experiment == 1)
sm2$abs_pos <- abs(sm2$Init.Lane.Pos)

### Unit conversions
sm2$BAC <- sm2$BAC*100

## Minor
res <- glmer(any_dept ~ THC  + BAC + scale(Avg.Speed) + SD.Speed + abs_pos + (1 | ID) , data = sm2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(res)
AIC(res)  ## AIC 916 w/o int, 918 w/ int
exp(coef(res)$`ID`[1,])
exp(confint(res, method = "Wald"))

## Major
#res <- glmer(major_dept ~ THC  + BAC + scale(Avg.Speed) + SD.Speed + abs_pos + (1 | ID) , data = sm2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
res <- glm(major_dept ~ THC  + BAC + THC:BAC + scale(Avg.Speed) + SD.Speed + abs_pos, data = sm2, family = "binomial")
summary(res)
AIC(res) ## AIC 577.3 w/o int, 570.8 w/ int
exp(coef(res))
exp(confint(res, method = "Wald"))


## Severe
#res <- glmer(sev_dept ~ THC  + BAC + scale(Avg.Speed) + SD.Speed + abs_pos + (1 | ID) , data = sm2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
res <- glm(sev_dept ~ THC  + BAC + scale(Avg.Speed) , data = sm2, family = "binomial")
summary(res)
AIC(res)  ## AIC 22 w/o int, 24 w/ int
exp(coef(res))
exp(confint(res, method = "Wald"))


#######################################################
########## PART 2 --- Duration of Departures ##########
#######################################################

### Any
sm3 <- dplyr::filter(sm2, Lane.Departure.Minor > 0)
m <- lmer(Lane.Departure.Minor ~ THC+ BAC + scale(Avg.Speed) + abs_pos +  (1 |ID), data = sm3, REML = FALSE, control = lmerControl(
            optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
#m <- lm(Lane.Departure.Minor ~ THC+ BAC + scale(Avg.Speed) + abs_pos, data = sm3)
summary(m)
confint(m, method = "Wald")

### Major
sm3 <- dplyr::filter(sm2, Lane.Departure.Major > 0)
m <- lmer(Lane.Departure.Major ~ THC+ BAC + scale(Avg.Speed) + abs_pos + (1 |ID), data = sm3, REML = FALSE, control = lmerControl(
            optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
#m <- lm(Lane.Departure.Major ~ THC+ BAC + scale(Avg.Speed) + abs_pos, data = ld2)
summary(m)
confint(m, method = "Wald")


### Severe
sm3 <- dplyr::filter(sm2, Lane.Departure.Severe > 0)
#m <- lmer(Lane.Departure.Severe ~ THC+ BAC + scale(Avg.Speed) + abs_pos + SD.Speed + (1 |ID), data = sm3, REML = FALSE, 
#          control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m <- lm(Lane.Departure.Severe ~ THC+ BAC + scale(Avg.Speed) , data = sm3)
#summary(m)


#####################################################
############### Message #############################
#####################################################
mr <- read.csv("H:\\NIDA\\final\\finalFinalMessage.csv")

## Binary outcomes
mr$any_dept <- ifelse(mr$Lane.Departure.Minor > 0, 1, 0)
mr$major_dept <- ifelse(mr$Lane.Departure.Major > 0, 1, 0)
mr$sev_dept <- ifelse(mr$Lane.Departure.Severe > 0, 1, 0)

## Exp period only, add initial position
mr2 <- dplyr::filter(mr, Experiment == 1)
mr2$abs_pos <- abs(mr2$Init.Lane.Pos)

### Unit conversions
mr2$BAC <- mr2$BAC*100

## Minor
res <- glmer(any_dept ~ THC  + BAC + scale(Avg.Speed) + abs_pos + Sd.Speed + (1 | ID) , data = mr2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(res)
AIC(res)  ## AIC 
exp(coef(res)$`ID`[1,])
exp(confint(res, method = "Wald"))

## Major
res <- glmer(major_dept ~ THC  + BAC + scale(Avg.Speed)  + abs_pos + Sd.Speed + (1 | ID) , data = mr2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
#res <- glm(major_dept ~ THC  + BAC + THC:BAC + scale(Avg.Speed) + SD.Speed + abs_pos, data = mr2, family = "binomial")
summary(res)
AIC(res) ## AIC 
exp(coef(res)$`ID`[1,])
exp(confint(res, method = "Wald"))

## Severe
#res <- glmer(sev_dept ~ THC  + BAC + scale(Avg.Speed)  + abs_pos + (1 | ID) , data = mr2, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
res <- glm(sev_dept ~ THC  + BAC + scale(Avg.Speed) + abs_pos, data = mr2, family = "binomial")
summary(res)
AIC(res)  ## AIC
exp(coef(res))
exp(confint(res, method = "Wald"))

#######################################################
########## PART 2 --- Duration of Departures ##########
#######################################################

### Any
mr3 <- dplyr::filter(mr2, Lane.Departure.Minor > 0)
m <- lmer(Lane.Departure.Minor ~ THC+ BAC + scale(Avg.Speed) + abs_pos  + Sd.Speed  +  (1 |ID), data = mr3, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
#m <- lm(Lane.Departure.Minor ~ THC+ BAC + scale(Avg.Speed) + abs_pos, data = sm3)
summary(m)
confint(m, method = "Wald")

### Major
mr3 <- dplyr::filter(mr2, Lane.Departure.Major > 0)
m <- lmer(Lane.Departure.Major ~ THC+ BAC + scale(Avg.Speed) + abs_pos + Sd.Speed   + (1 |ID), data = mr3, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
#m <- lm(Lane.Departure.Major ~ THC+ BAC + scale(Avg.Speed) + abs_pos, data = ld2)
summary(m)
confint(m, method = "Wald")

### Severe
mr3 <- dplyr::filter(mr2, Lane.Departure.Severe > 0)
#m <- lmer(Lane.Departure.Severe ~ THC+ BAC + scale(Avg.Speed) + abs_pos + SD.Speed + (1 |ID), data = sm3, REML = FALSE, 
#          control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m <- lm(Lane.Departure.Severe ~ THC+ BAC, data = mr3)
summary(m)

######################################################
############# Descriptives ###########################
######################################################

table(ld$DosingLevel, ld$any_dept, ld$Experiment) + table(sm$DosingLevel, sm$any_dept, sm$Experiment) + table(mr$DosingLevel, mr$any_dept, mr$Experiment)

table(ld$DosingLevel, ld$major_dept, ld$Experiment) + table(sm$DosingLevel, sm$major_dept, sm$Experiment) + table(mr$DosingLevel, mr$major_dept, mr$Experiment)

table(ld$DosingLevel, ld$sev_dept, ld$Experiment) + table(sm$DosingLevel, sm$sev_dept, sm$Experiment)  + table(mr$DosingLevel, mr$sev_dept, mr$Experiment)


mr3 <- dplyr::filter(mr2, Lane.Departure.Severe > 0)
sm3 <- dplyr::filter(sm2, Lane.Departure.Severe > 0)
ld3 <- dplyr::filter(ld2, Lane.Departure.Severe > 0)
mr3$DosingLevel
sm3$DosingLevel
ld3$DosingLevel

mr3$ID
sm3$ID
ld3$ID

nrow(ld) + nrow(sm) +nrow(mr)


##########################################################
############# Does INIT.POS vary by THC/BAC ##############
##########################################################

## Msg
m <- lmer(abs_pos ~ THC+ BAC + factor(LogStreams.5) + (1 |ID), data = mr2, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)

## Mirror
m <- lmer(abs_pos ~ THC+ BAC  + factor(LogStreams.5) + (1 |ID), data = sm2, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)

## Artist
m <- lmer(abs_pos ~ THC+ BAC + (1 |ID), data = ld2, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)
