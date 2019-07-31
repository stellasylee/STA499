# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("lqmm")

library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
library(plotly)
library(lqmm)

#final Mpdels
# Mirror Task ====
mirror <- read.csv ("H:\\NIDA\\analysisMirror.csv") # this already changed 299 frames to valid = 0 


boxplot(data = mirror, total ~ valid)  ## There is one very bad value, but it looks like you're removing it
mirror %>% group_by(valid) %>% summarize(max_val = max(total)) 
ggplot(filter(mirror, valid == 1), aes(x = factor(ID), y = total)) + geom_jitter()  ## Values at 298 might be questionable?
sum(mirror$total == 298)  ## I checked and switching to invalid doesn't change much

mirror <- dplyr::filter (mirror, total <= 300)
normalMirror <- filter(mirror, !(ID == 123 & Visit == 1)) # I think that the other visits are okay for ID 123?

######################################################################################
###### NEED TO FILTTER TO ONLY EXPERIMENT TO AVOID DOUBLE COUNTING EVERYTHING ########
######################################################################################
normalMirrorExp <- filter(normalMirror, Experiment == 1)

# Completion (switching to valid == 1, since this means completed?)
fit <- glmer((valid == 1) ~ BAC + THC  + (1 | ID) + (Visit == 1), data = normalMirrorExp, family = "binomial")
summary(fit)

complete <- filter(normalMirrorExp, valid == 1) 

# Time
fit <- lmer (total ~ BAC + THC + BAC:THC + (1 | ID) + (Visit == 1) + factor(LogStreams.5), data = complete, REML = FALSE)
summary(fit)
AIC (fit) # 29557.35  -> 14842.4 (Interaction increases AIC, not necessary)


### Conclusions = Neither BAC or THC associated with succesful completion or completion speed



# # General Driving Perf ---- Not using for now
# fit <- lmer(SD.Lane.Deviation ~ factor(Experiment) +  BAC + THC  + Avg.Speed  +  (1| ID) + factor (Visit) + factor(LogStreams.5), data = complete)
# fit <- lmer(Avg.Speed ~ factor(Experiment)  + BAC  + THC + BAC:THC +  (1| ID) + factor (Visit) + factor(LogStreams.5), data = complete)
# fit <- lmer(SD.Speed ~ factor(Experiment) +  BAC + THC+ (1| ID) + (Visit == 1) + factor(LogStreams.5), data = complete)
# 

# Baseline Driving perf
cntlMirror <- filter(normalMirror, Experiment == 0, valid == 1)

library(splines)
fit <- lmer(SD.Lane.Deviation ~ BAC + THC + ns(Avg.Speed,3)  +  (1| ID)  + factor(LogStreams.5),data = cntlMirror, REML = FALSE)
summary(fit)
AIC(fit) # 998.6346 -> 991.24 without (Visit == 1) -> 988.2864 using natural spline for Avg.Speed

fit <- lmer(Avg.Speed ~ BAC  + THC  + (1| ID) + (Visit == 1) + factor(LogStreams.5), data = cntlMirror, REML = FALSE)
summary(fit)
AIC(fit) # I Found that interaction does not improve AIC for this comparison

fit <- lmer(SD.Speed ~ BAC + THC  + (1| ID) + (Visit == 1) + factor(LogStreams.5), data = cntlMirror, REML = FALSE)
summary(fit)
AIC(fit) # 761.1331

### Conclusions = higher BAC -> worse lane keeping, higher average speed, increased variability in speed
###               higher THC -> no difference in lane keeping, slower average speed, no difference in speed variability
###               no interactions between THC:BAC, additive effects


# Paired ----
complete <- filter(mirror, valid == 1)
completePairMirror <- complete[complete$Experiment == 1, c(1:4, 6, 9, 13:16)]
for (i in 1:(nrow(complete)/2)){
  lane <- complete$SD.Lane.Deviation[(2*i-1)] - complete$SD.Lane.Deviation[(2*i)]
  speed <- complete$Avg.Speed[(2*i-1)] - complete$Avg.Speed[(2*i)]
  sdspeed <- complete$SD.Speed[(2*i-1)] - complete$SD.Speed[(2*i)]
  completePairMirror[i,"diffSDLane"] <- lane
  completePairMirror[i,"diffAvgSpeed"] <- speed
  completePairMirror[i,"diffSDSpeed"] <- sdspeed
}

d1 <- filter(complete, Experiment == 1)
d2 <- filter(complete, Experiment == 0)

head(d1)
head(d2)
pairedMirror <- data.frame(d2, diffSDLane = d1$SD.Lane.Deviation - d2$SD.Lane.Deviation,
                           diffAvgSpeed = d1$Avg.Speed - d2$Avg.Speed,
                           diffSDSpeed = d1$SD.Speed - d2$SD.Speed)


fit <- lmer(diffSDLane ~ BAC + THC  + diffAvgSpeed  +  (1| ID)  + (Visit == 1) +  factor(LogStreams.5), data = pairedMirror, REML = FALSE)
AIC(fit)
summary(fit)

fit <- lmer(diffAvgSpeed ~ BAC + THC +  (1| ID)  +  (Visit == 1) +  factor(LogStreams.5), data = pairedMirror, REML = FALSE)
summary(fit) ## ^ Degenerate random effect, use lm()

fit <- lm(diffAvgSpeed ~ BAC + THC + (Visit == 1) + Avg.Speed + factor(LogStreams.5), data = pairedMirror)
summary(fit)

fit <- lmer(diffSDSpeed ~ BAC + THC +  (1| ID) + (Visit == 1), data = completePairMirror, REML = FALSE)
summary(fit)
AIC(fit)

### Conclusions = No change in lane keeping, higher THC -> slowed down by more, no change in speed variability


#Artist Task ====
#loading required data
#reading in files
AnalysisArtist <- read.csv("H:\\NIDA\\analysisArtistNoEngage.csv")
ArtistTimes <- read.csv("H:\\NIDA\\artistTimes.csv")


#filtering out and creating separate dataframes for experimental and control groups

ArtistExperiment <- filter(AnalysisArtist, Experiment == "1" )
ArtistControl <- filter(AnalysisArtist, Experiment == "0")

#sorting these dataframes to ensure that the the rows in both of them align

ArtistExperiment <- arrange(ArtistExperiment, desc(DaqName, eventNum))
ArtistControl <- arrange(ArtistControl, desc(DaqName, eventNum))

ArtistExperiment <- mutate(ArtistExperiment, SD.Lane.Diff = ArtistExperiment$SD.Lane.Deviation - ArtistControl$SD.Lane.Deviation,
                           Avg.Speed.Diff = ArtistExperiment$Avg.Speed - ArtistControl$Avg.Speed, 
                           SD.Speed.Diff = ArtistExperiment$SD.Speed - ArtistControl$SD.Speed)

#filtering to keep only valid artist from ArtistExperiment
validArtist <- filter(AnalysisArtist, AnalysisArtist$valid > 0)

#filtering to keep only engaged data
keepingengagement <- dplyr::filter(ArtistExperiment, !((valid == 0) & (incorrect == 0)))


##########General performance Models
##artist task


####################################################################
############# NEED TO REMOVE CONTROL SEGS TO AVOID DOUBLE COUNTING!!
####################################################################

ArtistComp <- filter(AnalysisArtist, Experiment == 1)

#Performance on task measures
fit1 <- glm(data = ArtistComp, incorrect ~ THC + BAC, family = "binomial")
summary(fit1)
AIC(fit1)
exp(coef(fit1))


fit2 <- glmer(data = AnalysisArtist, valid ~ THC + BAC + (1|ID) + (pageNum == 1), family = "binomial")
#### ^ Changed to mixed effects model, here subj specific intercept matters
summary(fit2)
AIC(fit2)
exp(coef(fit2))

# #general models -- Not using for now
# #lane Deviation
# fit <- lmer(data = validArtist, log(SD.Lane.Deviation) ~ (1 | ID) + Experiment + THC + BAC + Avg.Speed + factor(pageNum))
# summary(fit)
# anova(fit)
# AIC(fit)
# #Avg Speed
# fit <- lmer(data = validArtist,  Avg.Speed ~ (1 | ID) + Experiment + THC + BAC + experimentallength + factor(pageNum))
# summary(fit)
# AIC(mesfit)
# #SD Speed
# fit <- lmer(data = validArtist,  SD.Speed ~ (1 | ID) + Experiment +  THC + BAC + experimentallength  + factor(pageNum))
# summary(fit)

### Baseline models
ArtistControl <- filter(AnalysisArtist, Experiment == 0)

fit <- lmer(data = ArtistControl, SD.Lane.Deviation ~ (1 | ID)  + THC + BAC + ns(Avg.Speed,3), REML = FALSE)
AIC(fit)
summary(fit)

fit <- lmer(data = ArtistControl, Avg.Speed ~ (1 | ID)  + THC + BAC, REML = FALSE)
AIC(fit)
summary(fit)

fit <- lmer(data = ArtistControl, SD.Speed ~ (1 | ID)  + THC + BAC, REML = FALSE)
AIC(fit)
summary(fit)


### Conclusions = No differences in baseline driving


#### Paired models
validArtist <- filter(ArtistExperiment, valid > 0)

#lane deviation
fit <- lmer(data = validArtist, SD.Lane.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + (pageNum == 1), REML = FALSE)
summary(fit)
AIC(fit)

#average speed
#fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + (pageNum == 1))
summary(fit)  ## ^ Model doesn't need random effect
AIC(fit)

fit <- lm(data = validArtist,  Avg.Speed.Diff ~ + THC + BAC + (pageNum == 1) + Avg.Speed)
summary(fit)

#SD Speed
fit <- lm(data = validArtist, SD.Speed.Diff ~ THC + BAC + Avg.Speed + (pageNum == 1))
summary(fit)
AIC(fit)

### Conclusions no detectable change in driving performance while engaged in task



##message task

#reading in message task files
#eventTimesMessage <- read.csv("H:\\CannabisStudy\\message\\eventTimesMessage.csv")
analysisMes <- read.csv("H:\\NIDA\\analysisMesBrake.csv")

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


### Baseline characteristics

#lane Deviation
mesfit <- lmer(data = MessageControl, SD.Lane.Deviation ~ (1 | ID) + THC + BAC +
                 ns(Avg.Speed,3) + factor(LogStreams.5), REML = FALSE) 
AIC(mesfit)
summary(mesfit)
anova(mesfit)

#Average speed
mesfit <- lmer(data = MessageControl, Avg.Speed ~ (1 | ID) + THC + BAC +  factor(LogStreams.5))
summary(mesfit)
AIC(mesfit)

#SD Speed
mesfit <- lmer(data = analysisMes,  Sd.Speed ~ (1 | ID) + THC + BAC + factor(LogStreams.5))
summary(mesfit)
AIC(mesfit)


## Conclusions = No effect on lane keeping, higher THC = slower speeds, no effect on speed variation

## Paired Difference models

#message task
mesfit <- lmer(data = MessageExperiment, SD.Lane.Diff ~ (1 | ID) + THC  + BAC + Avg.Speed  + factor(LogStreams.5), REML = FALSE)
summary(mesfit)
AIC(mesfit)

#Average Speed
mesfit <- lmer(data = MessageExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(LogStreams.5), REML = FALSE)
summary(mesfit)
AIC(mesfit)

#SD Speed
mesfit <- lmer(data = MessageExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC   + factor(LogStreams.5), REML = FALSE) 
summary(mesfit)
AIC(mesfit)

## Conclusions = BAC worsens lane keeping during task, higher THC slows down by less and might have more speed variation