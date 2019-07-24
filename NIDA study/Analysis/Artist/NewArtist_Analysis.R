install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lme4")
install.packages("lmerTest")
install.packages("lqmm")

library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
library(plotly)
library(lqmm)
#reading in files

AnalysisArtist <- read.csv("H:\\CannabisStudy\\artist\\artistTaskAnalysis.csv")
ArtistTimes <- read.csv("H:\\CannabisStudy\\artist\\artistTimes.csv")

#converting the length of experimental segment into seconds from frames
AnalysisArtist$experimentallength <- (AnalysisArtist$experimentallength /60)

#filtering out and creating separate dataframes for experimental and control groups

ArtistExperiment <- filter(AnalysisArtist, Experiment == "1" )

ArtistControl <- filter(AnalysisArtist, Experiment == "0")

#sorting these dataframes to ensure that the the rows in both of them align

ArtistExperiment <- arrange(ArtistExperiment, desc(DaqName, eventNum))
ArtistControl <- arrange(ArtistControl, desc(DaqName, eventNum))

ArtistExperiment <- mutate(ArtistExperiment, SD.Lane.Diff = ArtistExperiment$SD.Lane.Deviation - ArtistControl$SD.Lane.Deviation,
                         Avg.Speed.Diff = ArtistExperiment$Avg.Speed - ArtistControl$Avg.Speed, 
                         SD.Speed.Diff = ArtistExperiment$SD.Speed - ArtistControl$SD.Speed)

#modelling the data with ArtistExperiment

#lane deviation
fit <- lmer(data = ArtistExperiment, SD.Lane.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#average speed
fit <- lmer(data = ArtistExperiment, Avg.Speed.Diff ~ (1 | ID) + THC + BAC + factor(pageNum) + experimentallength)
summary(fit)

#SD Speed
fit <- lmer(data = ArtistExperiment, SD.Speed.Diff ~ (1 | ID) + THC + BAC + Avg.Speed + factor(pageNum) + experimentallength)
summary(fit)

#modelling the data with quantile regression with mixed effects

#lane deviation
mesfit <- lqmm(SD.Lane.Diff ~ THC + BAC + factor(pageNum) + Avg.Speed, random = ~ 1, group = ID, data = ArtistExperiment,
               tau = 0.75)
summary(mesfit)

#Avg.Speed
mesfit <- lqmm(Avg.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~ 1, group = ID, data = ArtistExperiment,
               tau = 0.75)
summary(mesfit)

#SD.Speed
mesfit <- lqmm(SD.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~ 1, group = ID, data = ArtistExperiment,
               tau = 0.75)
summary(mesfit)

#filtering to keep only valid artist from ArtistExperiment

validArtist <- dplyr::filter(ArtistExperiment, ArtistExperiment$valid > 0)


#modelling with median as well
#Lane deviation with median
fit <- lqmm(SD.Lane.Diff ~ THC + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#Lane deviation with two THC groups with median

fit <- lqmm(SD.Lane.Diff ~ (THC == 0) + THC + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)

summary(fit)

#Lane Deviation with two THC groups with mean

fit <- lmer(data = validArtist, SD.Lane.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Lane deviation with log(THC + 1) with mean
fit <- lmer(data = validArtist, SD.Lane.Diff ~ (1 | ID) + log(THC + 1) + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Lane deviation with log(THC + 1 with median)
fit <- lqmm(SD.Lane.Diff ~ log(THC + 1) + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)


#Average speed

#average speed with median

fit <- lqmm(Avg.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)
###### THC marginally significant with positive coefficient#########

#two THC groups with median

fit <- lqmm(Avg.Speed.Diff ~ (THC == 0) + THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

###### THC significant with positive coeffecient####

#two THC groups with mean

fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)

#Log(THC + 1 with mean)
fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + log(THC + 1) + BAC + experimentallength + factor(pageNum))

summary(fit)

##Log(THC + 1 with median)


fit <- lqmm(Avg.Speed.Diff ~ log(THC + 1) + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#SD Speed 

#SD speed with median

fit <- lqmm(SD.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with median
fit <- lqmm(SD.Speed.Diff ~ (THC == 0) + THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with mean

fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)

#Log(THC + 1 with mean)
fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + log(THC + 1) + BAC + experimentallength + factor(pageNum))

summary(fit)

##Log(THC + 1 with median)

fit <- lqmm(SD.Speed.Diff ~ log(THC + 1) + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)




#modelling data with keeping engagement
keepingengagement <- dplyr::filter(ArtistExperiment, !((valid == 0) & (incorrect == 0)))

#Lane deviation
fit <- lqmm(SD.Lane.Diff ~ THC + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = keepingengagement, tau = 0.5)
summary(fit)

#Lane deviation with two THC groups with median

fit <- lqmm(SD.Lane.Diff ~ (THC == 0) + THC + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = keepingengagement, tau = 0.5)

summary(fit)

#Lane Deviation with two THC groups with mean

#Lane deviation with median
fit <- lmer(data = keepingengagement, SD.Lane.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Lane deviation with log(THC + 1) with mean
fit <- lmer(data = keepingengagement, SD.Lane.Diff ~ (1 | ID) + log(THC + 1) + BAC + Avg.Speed + factor(pageNum))
summary(fit)

#Lane deviation with log(THC + 1 with median)
fit <- lqmm(SD.Lane.Diff ~ log(THC + 1) + BAC + Avg.Speed + factor(pageNum), random = ~1, group = ID, 
            data = keepingengagement, tau = 0.5)
summary(fit)

#average speed

#average speed with median

fit <- lqmm(Avg.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5, control = lqmmControl(LP_max_iter = 10000))
summary(fit)
####### THC significant with positive coefficient############# 

#two THC groups with median

fit <- lqmm(Avg.Speed.Diff ~ (THC == 0) + THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

##THC marginally sig with positive coeff##

#two THC groups with mean

fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)

#Log(THC + 1 with mean)
fit <- lmer(data = validArtist, Avg.Speed.Diff ~ (1 | ID) + log(THC + 1) + BAC + experimentallength + factor(pageNum))

summary(fit)

##Log(THC + 1 with median)

fit <- lqmm(Avg.Speed.Diff ~ log(THC + 1) + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)


#SD Speed

#SD speed with median

fit <- lqmm(SD.Speed.Diff ~ THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with median
fit <- lqmm(SD.Speed.Diff ~ (THC == 0) + THC + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#two THC groups with mean

fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + (THC == 0) + THC + BAC + experimentallength + factor(pageNum))
summary(fit)

#Log(THC + 1 with mean)
fit <- lmer(data = validArtist, SD.Speed.Diff ~ (1 | ID) + log(THC + 1) + BAC + experimentallength + factor(pageNum))

summary(fit)

##Log(THC + 1 with median)

fit <- lqmm(SD.Speed.Diff ~ log(THC + 1) + BAC + experimentallength + factor(pageNum), random = ~1, group = ID, 
            data = validArtist, tau = 0.5)
summary(fit)

#looking at the plots
ggplot(data = validArtist, aes (x = THC, y = SD.Lane.Deviation, color = Avg.Speed)) + 
  geom_point() + scale_color_continuous(low = "blue", high = "red") + facet_wrap(~ ID, scales = 'free') + geom_smooth()



#people with higher levels of THC required longer to complete the artist task
ggplot(data = validArtist, aes (x = THC, y = experimentallength)) + 
  geom_point()  


#Clustering

THCrangeart <- NULL
for (i in unique(validArtist$ID)){
  temp <- filter(validArtist, ID == i)
  THCrangeart <- rbind.data.frame(THCrangeart, c(i, max(temp$THC), min(temp$THC), (max(temp$THC) - min(temp$THC))))
}
colnames(THCrangeart) <- c("ID", "max", "min", "range")
rownames(THCrangeart) <- THCrangeart$ID
k2 <- kmeans(THCrangeart[, 4], centers = 2, nstart = 25)
fviz_cluster(k2, data = THCrangeart)
# Cluster 1: Participant 7, 18, 29, 34, 35, 123, 129
k3 <- kmeans(THCrangeart[, 4], centers = 3, nstart = 25)
fviz_cluster(k3, data = THCrangeart)
# Cluster 1: Participant 7, 18
# cluster 2: Participant 25, 26, 29, 32, 34, 35, 123, 129

#Creating the clusters (NOTE: These clusters are created using valid artist)
clus1art <- dplyr::filter(validArtist, ID == "7" | ID == "18" | 
                         ID == "29" | ID == "34" | ID == "120" | ID == "123")

clus2art <- dplyr::filter(validArtist, ID == "3" | ID == "10" | ID == "15" | ID == "17" |
                         ID == "15" | ID == "21" | ID == "25" | ID == "26" | ID == "31" | ID == "32"
                       | ID == "35" | ID == "104" | ID == "113" | ID == "129")

#creating models with the clusters

#Lane Deviation
fit <- lmer(data = clus1art, SD.Lane.Diff ~ (1 | ID) + ns(THC,3) + BAC + ns(Avg.Speed,3) + factor(pageNum))
summary(fit)
anova(fit)
AIC(fit)

fit <- lmer(data = clus2art, SD.Lane.Diff ~ (1 | ID) + ns(THC,3) + BAC + ns(Avg.Speed,3) + factor(pageNum))
summary(fit)
anova(fit)

#Avg. Speed
fit <- lmer(data = clus1art, Avg.Speed.Diff ~ (1 | ID) + ns(THC,3) + BAC  + factor(pageNum))
summary(fit)
anova(fit)

fit <- lmer(data = clus2art, Avg.Speed.Diff ~ (1 | ID) + ns(THC,3) + BAC  + factor(pageNum))
summary(fit)
anova(fit)

#SD.Speed
fit <- lmer(data = clus1art, SD.Speed.Diff ~ (1 | ID) + ns(THC,3) + BAC  + factor(pageNum))
summary(fit)
anova(fit)

fit <- lmer(data = clus2art, SD.Speed.Diff ~ (1 | ID) + ns(THC,3) + BAC  + factor(pageNum))
summary(fit)
anova(fit)
