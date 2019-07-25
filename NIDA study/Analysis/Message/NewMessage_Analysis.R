install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("foreach")
install.packages("lme4")
install.packages("lqmm")

library(MASS)
library(lqmm)
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
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
###### Plotting predicted values with spline
#lane deviation

mesfit <- lmer(data = MessageExperiment, SD.Lane.Diff ~ (1 | ID) + ns(THC, 3) + BAC + Avg.Speed  + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
### BAC is significant predictor of lane deviation####

mm <- with(MessageExperiment, model.matrix( ~ -1 + ns(THC,3))) # Recreate basis expansions of THC
yy <- as.matrix(coef(mesfit)$ID[1,2:4]) %*% t(as.matrix(mm))
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()


#w/o spline
mesfit <- lmer(data = MessageExperiment, SD.Lane.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(LogStreams.5))
mm <- with(MessageExperiment, model.matrix( ~ -1 + THC)) 
yy <- as.matrix(coef(mesfit)$ID[1,2]) %*% t(as.matrix(mm)) 
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()
summary(mesfit)
anova(mesfit)
### BAC is significant predictor of lane deviation####


#Average Speed
mesfit <- lmer(data = MessageExperiment, Avg.Speed.Diff ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5)) 
summary(mesfit)
anova(mesfit)

mm <- with(MessageExperiment, model.matrix( ~ -1 + ns(THC,3))) # Recreate basis expansions of THC
yy <- as.matrix(coef(mesfit)$ID[1,2:4]) %*% t(as.matrix(mm))
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()


#w/o spline
mesfit <- lmer(data = MessageExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)

mm <- with(MessageExperiment, model.matrix( ~ -1 + THC)) 
yy <- as.matrix(coef(mesfit)$ID[1,2]) %*% t(as.matrix(mm)) 
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()
#### THC significant predictor with positive coefficient#### i.e. slow down less

#standard deviation speed
mesfit <- lmer(data = MessageExperiment, SD.Speed.Diff ~ (1 | ID) + ns(THC,3) + BAC + factor(LogStreams.5)) 
summary(mesfit)
anova(mesfit)

mm <- with(MessageExperiment, model.matrix( ~ -1 + ns(THC,3))) # Recreate basis expansions of THC
yy <- as.matrix(coef(mesfit)$ID[1,2:4]) %*% t(as.matrix(mm))
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()


#w/o spline
mesfit <- lmer(data = MessageExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5)) 
summary(mesfit)
anova(mesfit)

mm <- with(MessageExperiment, model.matrix( ~ -1 + THC)) 
yy <- as.matrix(coef(mesfit)$ID[1,2]) %*% t(as.matrix(mm)) 
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()


#max brake force

mesfit <- lmer(data = MessageExperiment, Break.Diff ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5) + Avg.Speed) 
summary(mesfit)
anova(mesfit)

mm <- with(MessageExperiment, model.matrix( ~ -1 + ns(THC,3))) # Recreate basis expansions of THC
yy <- as.matrix(coef(mesfit)$ID[1,2:4]) %*% t(as.matrix(mm))
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()


#w/o spline
mesfit <- lmer(data = MessageExperiment, Break.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5) + Avg.Speed) 


mm <- with(MessageExperiment, model.matrix( ~ -1 + THC)) 
yy <- as.matrix(coef(mesfit)$ID[1,2]) %*% t(as.matrix(mm)) 
MessageExperiment$yy <- as.vector(yy)
ggplot(data = MessageExperiment, aes(x = THC, y = yy, color = factor(ID %in% c(7, 18, 29, 34, 120, 123)))) + geom_point()
ggplot(data = MessageExperiment, aes(x = THC, y = Break.Diff))
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

mm <- with(MessageExperiment, model.matrix( ~ -1 + ns(THC,3))) # Recreate basis expansions of THC
yy <- as.matrix(coef(mesfit)$ID[1,3:5]) %*% t(as.matrix(mm))
plot(messagePair$THC, yy)
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

###clustering data
install.packages("factoextra")
library(factoextra)

THCrange <- NULL
for (i in unique(MessageExperiment$ID)){
  temp <- filter(MessageExperiment, ID == i)
  THCrange <- rbind.data.frame(THCrange, c(i, max(temp$THC), min(temp$THC), (max(temp$THC) - min(temp$THC))))
}
colnames(THCrange) <- c("ID", "max", "min", "range")
rownames(THCrange) <- THCrange$ID
k2 <- kmeans(THCrange[, 4], centers = 2, nstart = 25)
fviz_cluster(k2, data = THCrange)
# Cluster 1: Participant 7, 18, 29, 34, 120, 123

k3 <- kmeans(THCrange[, 4], centers = 3, nstart = 25)
fviz_cluster(k3, data = THCrange)
# Cluster 1: Participant 7, 18
# cluster 2: Participant 29, 31, 34, 120, 123

#Creating the clusters
clus1 <- dplyr::filter(MessageExperiment, ID == "7" | ID == "18" | 
                      ID == "29" | ID == "34" | ID == "120" | ID == "123")

clus2 <- dplyr::filter(MessageExperiment, ID == "3" | ID == "10" | ID == "15" | ID == "17" |
                         ID == "15" | ID == "21" | ID == "25" | ID == "26" | ID == "31" | ID == "32"
                       | ID == "35" | ID == "104" | ID == "113" | ID == "129")


library(splines)
fit <- lmer(log(SD.Lane.Deviation) ~ factor(Experiment) + factor (pageNum) + BAC + ns(THC, 3) + ns(Avg.Speed, 3)  +  (1| ID) , data = validArtist)

#modelling the data with clusters
library(splines)

#Lane Deviation
mesfit <- lmer(data = clus1, SD.Lane.Deviation ~ (1 | ID) + ns(THC, 3) + BAC + Avg.Speed.Diff + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)

mesfit <- lmer(data = clus2, SD.Lane.Deviation ~ (1 | ID) + THC + BAC + Avg.Speed.Diff + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
##BAC sig with positive coeff
##Spline does not improve the model

#Average Speed
mesfit <- lmer(data = clus1, Avg.Speed ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)


mesfit <- lmer(data = clus2, Avg.Speed ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
##BAC significant with positive coefficient

#SD.Speed
mesfit <- lmer(data = clus1, Avg.Speed ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)

mesfit <- lmer(data = clus2, Avg.Speed ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
#Max Brake diff
mesfit <- lmer(data = clus1, Max.Brake ~ (1 | ID) + ns(THC, 3) + BAC + Avg.Speed + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)

mesfit <- lmer(data = clus2, Max.Brake ~ (1 | ID) + ns(THC, 3) + BAC + ns(Avg.Speed,3) + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)




#Paired Difference models
#Lane Deviation
mesfit <- lmer(data = clus1, SD.Lane.Diff ~ (1 | ID) + ns(THC, 3) + BAC + THC:BAC + Avg.Speed.Diff + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
##### THC:BAC sig interaction

mesfit <- lmer(data = clus2, SD.Lane.Diff ~ (1 | ID) + ns(THC, 3) + BAC  + Avg.Speed.Diff + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
###BAC significant with positive coefficient

#Average Speed
mesfit <- lmer(data = clus1, Avg.Speed.Diff ~ (1 | ID) + ns(THC, 3) + BAC  + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)


mesfit <- lmer(data = clus2, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)

#SD.Speed
mesfit <- lmer(data = clus1, SD.Speed.Diff ~ (1 | ID) + ns(THC, 3) + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)


mesfit <- lmer(data = clus2, SD.Speed.Diff ~ (1 | ID) + THC + BAC + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
#Spline does not make the model better

#Max Brake diff
mesfit <- lmer(data = clus1, Break.Diff ~ (1 | ID) + ns(THC, 3) + BAC + Avg.Speed.Diff + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)

mesfit <- lmer(data = clus2, Break.Diff ~ (1 | ID) + ns(THC, 3) + BAC + Avg.Speed.Diff + factor(LogStreams.5))
summary(mesfit)
anova(mesfit)
AIC(mesfit)
#THC significant for the second level, 


ggplot(data = clus1, aes (x = THC, y = SD.Lane.Diff, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ID , scales = 'free') + geom_smooth()
