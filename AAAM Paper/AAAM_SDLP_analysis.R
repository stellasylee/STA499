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
library(splines)


####################################################
############### Artist #############################
####################################################
ld <- read.csv("H:\\NIDA\\final\\finalFinalArtistLD.csv")
ld$ID <- factor(ld$ID)
levels(ld$ID) <- paste(1:19)

ld1 <- filter(ld, Experiment == 1)
ld2 <- filter(ld, Experiment == 0)

set.seed(123)
df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC + rnorm(nrow(ld1),0, sd = .21), control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, 
                  Change = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation, SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed,
                  pageNum = factor(ld1$pageNum == 1))
ggplot(df1) + geom_segment(aes(x = THC, y = control, xend = THC, yend = task, col = Change),
                           arrow = arrow(length = unit(0.2,"cm"))) +
              scale_color_gradient2(low = "green", high = "red", mid = "yellow") +
              theme_bw()   + geom_smooth(aes(x = THC, y = Change)) + labs(y = "SDLP", title = "Artist-Search")


set.seed(123)
ggplot(df1) + geom_text(aes(x = THC, y = Change, label = ID), cex = 3) +
  geom_smooth(aes(x = THC, y = Change), se = FALSE) + geom_smooth(aes(x = THC, y = Change), method = "lm", se = FALSE, lty = 2)


df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC, control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, 
                  Change = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation, SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed, Init.pos = abs(ld1$Init.Lane.Pos),
                  pageNum = factor(ld1$pageNum == 1), Init.Speed = ld2$Avg.Speed, valid = ld1$valid, incorrect = ld1$incorrect)

df1 <- filter(df1, valid == 1 | incorrect == 1)
#m <- lm(Change ~ ns(THC,3) + BAC + SpeedDec + pageNum + Init.Speed , data = df1)
m <- lmer(Change ~ ns(THC,3) + BAC + SpeedDec + pageNum + Init.Speed + Init.pos + (1 |ID), data = df1, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
anova(m)
summary(m)



### Let's plot it
mm <- with(df1, model.matrix( ~ -1 + ns(THC,3))) # Recreate basis expansions of THC
yy <- as.matrix(coef(m)$ID[1,2:4]) %*% t(as.matrix(mm)) # Apply coefficients from model fit to get predictions

### Plot these predictions
plot(df1$THC, yy)

visreg::visreg(m, xvar = "THC")

####################################################
############### Message #############################
####################################################
ld <- read.csv("H:\\NIDA\\final\\finalFinalMessage.csv")
ld$ID <- factor(ld$ID)
levels(ld$ID) <- paste(1:19)

ld1 <- filter(ld, Experiment == 1)
ld2 <- filter(ld, Experiment == 0)

set.seed(123)
df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC + rnorm(nrow(ld1),0, sd = .21), control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, 
                  Change = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation, SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed)
ggplot(df1) + geom_segment(aes(x = THC, y = control, xend = THC, yend = task, col = Change),
                           arrow = arrow(length = unit(0.2,"cm"))) +
  scale_color_gradient2(low = "green", high = "red", mid = "yellow") +
  theme_bw()   + geom_smooth(aes(x = THC, y = Change)) + labs(y = "SDLP", title = "Message-Reading")



set.seed(123)
ggplot(df1) + geom_text(aes(x = THC, y = Change, label = ID), cex = 3) +
  geom_smooth(aes(x = THC, y = Change), se = FALSE) + geom_smooth(aes(x = THC, y = Change), method = "lm", se = FALSE, lty = 2)


df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC, control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, Init.pos = abs(ld1$Init.Lane.Pos),
                  Change = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation, SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed, Init.Speed = ld2$Avg.Speed)

m <- lmer(Change ~ ns(THC,3) + BAC + Init.pos  +  (1 |ID), data = df1, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
anova(m)
summary(m)

####################################################
############### Mirror #############################
####################################################

ld <- read.csv("H:\\NIDA\\final\\finalFinalMirror.csv")
ld <- filter(ld, !(ID == 123 & Visit == 1))


ld1 <- filter(ld, Experiment == 1)
ld2 <- filter(ld, Experiment == 0)

set.seed(123)
df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC + rnorm(nrow(ld1),0, sd = .21), control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, 
                  Change = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation, SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed, Init.pos = abs(ld1$Init.Lane.Pos))
ggplot(df1) + geom_segment(aes(x = THC, y = control, xend = THC, yend = task, col = Change),
                           arrow = arrow(length = unit(0.2,"cm"))) +
  scale_color_gradient2(low = "green", high = "red", mid = "yellow") +
  theme_bw()   + geom_smooth(aes(x = THC, y = Change), method = "loess") + labs(y = "SDLP")

df1 <- data.frame(ID = ld1$ID, BAC = ld1$BAC, THC = ld1$THC, control = ld2$SD.Lane.Deviation, task = ld1$SD.Lane.Deviation, 
                  Change = ld1$SD.Lane.Deviation - ld2$SD.Lane.Deviation,Init.pos = abs(ld1$Init.Lane.Pos),
                  SpeedDec = ld1$Avg.Speed - ld2$Avg.Speed, Init.Speed = ld2$Avg.Speed,
                  Location = factor(ld1$LogStreams.5))

m <- lmer(Change ~ ns(THC,3) + BAC + SpeedDec  + Location + Init.pos + (1 |ID), data = df1, REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
anova(m)
summary(m)