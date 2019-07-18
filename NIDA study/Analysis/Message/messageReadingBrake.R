library(stringr)
library(tidyverse)
library ("readxl")
library ("dplyr")

# Load data
mes <- read.csv ("H:\\NIDA\\analysisMes.csv")
mesTime <- read.csv ("H:\\NIDA\\eventTimesMessage.csv")

fileNames <- mesTime$DaqName

# sort to match the order of each file
mes <- mes[order(mes$ID, mes$DosingLevel, mes$eventNum),]
mesTime <- mesTime[order(mesTime$ID, mesTime$DosingLevel, mesTime$eventNum),]

# Add brake force variable ----
for (i in 1:length(fileNames)){
  print(i)
  if (i == 1) { # shorten time for loading same file again
    file <- read.csv (paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  }else if (fileNames[i] != fileNames[(i-1)]){
    file <- read.csv (paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  }
  start <- mesTime$start[i]
  end <- mesTime$end[i]

  # max brake force during experiment condition
  mes[(2*i-1),"Max.Brake"] <- max(file$CFS.Brake.Pedal.Force[start:end])
  # max brake force during control condition
  mes[(2*i),"Max.Brake"] <-max(file$CFS.Brake.Pedal.Force[(start-(end-start)):start])
}

write.csv(mes, "H:\\NIDA\\analysisMesBrake.csv")

# Explore Max.Brake variable ----
exp <- mes[mes$Experiment == 1, ]
cntl <- mes[mes$Experiment == 0, ]

mesByExperiment <- group_by(mes, Experiment)
summarize(mesByExperiment, 
          min = min(Max.Brake, na.rm = TRUE), 
          Q1 = quantile(Max.Brake, .25, na.rm = TRUE), 
          median = median(Max.Brake, na.rm = TRUE),
          mean = mean(Max.Brake, na.rm = TRUE),
          Q3 = quantile(Max.Brake, .75, na.rm = TRUE), 
          max = max(Max.Brake, na.rm = TRUE))

# Paired difference 
messagePair <- mes[mes$Experiment == 1, ]
colnames(mes)[9] <- "SD.Speed"
for (i in 1:(nrow(mes)/2)){
  lane <- mes$SD.Lane.Deviation[(2*i-1)] - mes$SD.Lane.Deviation[(2*i)]
  speed <- mes$Avg.Speed[(2*i-1)] - mes$Avg.Speed[(2*i)]
  sdspeed <- mes$SD.Speed[(2*i-1)] - mes$SD.Speed[(2*i)]
  brake <- mes$Max.Brake[(2*i-1)] - mes$Max.Brake[(2*i)]
  messagePair[i,"diffSDLane"] <- lane
  messagePair[i,"diffAvgSpeed"] <- speed
  messagePair[i,"diffSDSpeed"] <- sdspeed
  messagePair[i,"diffBrake"] <- brake
}

summarize(messagePair, 
          min = min(Max.Brake, na.rm = TRUE), 
          Q1 = quantile(Max.Brake, .25, na.rm = TRUE), 
          median = median(Max.Brake, na.rm = TRUE),
          mean = mean(Max.Brake, na.rm = TRUE),
          Q3 = quantile(Max.Brake, .75, na.rm = TRUE), 
          max = max(Max.Brake, na.rm = TRUE))
# Difference in brake force is skewed
# participant 123 is also maximum 

messagePairByExperiment <- group_by(messagePair, DosingLevel)
summarize(messagePairByExperiment, 
          min = min(Max.Brake, na.rm = TRUE), 
          Q1 = quantile(Max.Brake, .25, na.rm = TRUE), 
          median = median(Max.Brake, na.rm = TRUE),
          mean = mean(Max.Brake, na.rm = TRUE),
          Q3 = quantile(Max.Brake, .75, na.rm = TRUE), 
          max = max(Max.Brake, na.rm = TRUE))

library(lme4)
library(lmerTest)

fit <- lmer(diffBrake ~ BAC + THC + (1| ID) + factor(LogStreams.5), data = messagePair)
summary(fit)
anova(fit)

fit <- lmer(diffAvgSpeed ~ BAC + THC + (1| ID) + diffBrake + factor(LogStreams.5), data = messagePair)
summary(fit)
anova(fit)
