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

## Scale BrAC
ld2$BAC <- ld2$BAC*100

## Task Time (amongst engaged)
m <- lmer(total/60 ~ THC+ BAC + factor(pageNum) + (1 |ID), data = ld2, REML = FALSE, control = lmerControl(
            optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)
confint(m, method = "Wald")

## Task time (amongst completed)
ld3 <- dplyr::filter(ld2, valid == 1)
m <- lmer(total/60 ~ THC+ BAC + factor(pageNum) + (1 |ID), data = ld3, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)
confint(m, method = "Wald")


## Incorrect/Non-completed

AnalysisArtist <- read.csv("H:\\NIDA\\final\\finalArtist.csv")
ArtistTimes <- read.csv("H:\\NIDA\\final\\artistTimes.csv")
ArtistExperiment <- filter(AnalysisArtist, Experiment == 1)
ArtistControl <- filter(AnalysisArtist, Experiment == 0)
#sorting these dataframes to ensure that the the rows in both of them align

ArtistExperiment <- arrange(ArtistExperiment, desc(DaqName, eventNum))
ArtistControl <- arrange(ArtistControl, desc(DaqName, eventNum))

ArtistExperiment <- mutate(ArtistExperiment, SD.Lane.Diff = ArtistExperiment$SD.Lane.Deviation - ArtistControl$SD.Lane.Deviation,
                           Avg.Speed.Diff = ArtistExperiment$Avg.Speed - ArtistControl$Avg.Speed, 
                           SD.Speed.Diff = ArtistExperiment$SD.Speed - ArtistControl$SD.Speed)

#filtering to keep only valid artist from ArtistExperiment
validArtist <- filter(AnalysisArtist, AnalysisArtist$valid > 0)

## Scale BrAC
ArtistExperiment$BAC <- ArtistExperiment$BAC*100

# Incorrect resp
fit1 <- glm(data = ArtistExperiment, incorrect ~ THC + BAC , family = "binomial")
summary(fit1)
AIC(fit1) # 342.2354
exp(coef(fit1))
exp(confint(fit1, method = "Wald"))

# Not completing the task
fit2 <- glmer(data = ArtistExperiment, (valid == 0) ~ THC + BAC + (1|ID) + (pageNum == 1), family = "binomial")
summary(fit2)
AIC(fit2) #  386.4043
1/exp(coef(fit2)$`ID`$THC[1])
1/exp(coef(fit2)$`ID`$BAC[1])
exp(confint(fit2, method = "Wald"))
1/exp(confint(fit2, method = "Wald"))


####################################################
############### Mirror #############################
####################################################
sm <- read.csv("H:\\NIDA\\final\\finalFinalMirror.csv")
## Exclude bad data
normalMirror <- filter(sm, !(ID == 123 & Visit == 1)) # I think that the other visits are okay for ID 123?
sm2 <- normalMirror

## Scale BrAC
sm2$BAC <- sm2$BAC*100

## Task Time (amongst completed)
sm3 <- dplyr::filter(sm2,Experiment == 1 & total <= 299)

m <- lmer(total/60 ~ THC+ BAC + factor( LogStreams.5) + (1 |ID), data = sm3, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(m)
confint(m, method = "Wald")

## Completion
sm2 <- dplyr::filter(sm2, Experiment == 1)
m <- glmer((valid == 1) ~ THC+ BAC + factor( substr(LogStreams.5,1,1)) + (1 |ID), data = sm2, family = "binomial")
summary(m)
exp(coef(m)$`ID`$THC[1])
exp(coef(m)$`ID`$BAC[1])
exp(confint(m, method = "Wald"))


### Completion diff by task
sm <- read.csv("H:\\NIDA\\final\\finalFinalMirror.csv")
ld <- read.csv("H:\\NIDA\\final\\finalFinalArtistLD.csv")

b <- rbind(data.frame(task = "SM", valid = sm$valid, time = sm$total),
      data.frame(task = "AR", valid = ld$valid, time = ld$total))

chisq.test(table(b$task, b$valid))

b2 <- filter(b, valid == 1)
summary(lm(time ~ task, data = b2))

## SM overall completion
2998/(216+2998)



##################################
##### SHIFTS #####################
##################################
ld <- read.csv("H:\\NIDA\\final\\finalFinalArtistLD.csv")
ld$ID <- factor(ld$ID)
levels(ld$ID) <- paste(1:19)

ld1 <- filter(ld, Experiment == 1)
ld2 <- filter(ld, Experiment == 0)

ld1 <- arrange(ld1, desc(DaqName, eventNum))
ld2 <- arrange(ld2, desc(DaqName, eventNum))


df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC, control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, 
                  LPChange = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation, SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed, 
                  SDSChange = ld1$SD.Speed - ld2$SD.Speed,
                  pageNum = factor(ld1$pageNum == 1), Init.Speed = ld2$Avg.Speed, valid = ld1$valid, incorrect = ld1$incorrect)

## Only valid
df2 <- filter(df1, valid == 1)

## SDLP
m <- lm(LPChange ~ THC + BAC + Init.Speed + pageNum, data = df2)
anova(m)
summary(m)

## Speed
m <- lmer(SpeedDec ~ THC + BAC + pageNum  + (1 |ID), data = df2, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
anova(m)
summary(m)

## SDS
m <- lm(SDSChange ~ THC + BAC  + pageNum, data = df2)
anova(m)
summary(m)




