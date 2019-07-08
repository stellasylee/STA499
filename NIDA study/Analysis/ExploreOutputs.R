library(stringr)
library(tidyverse)
library ("dplyr")
library(ggplot2)

outputs <- read.csv("H:\\NIDA\\analysisTasks3.csv")
outputs$Experiment <- as.factor(outputs$Experiment)

# Boxplots grouped by Experiments / Dosing Level
ggplot(outputs, aes(x = Experiment, y = SD.Lane.Deviation)) + geom_boxplot()
ggplot(outputs, aes(x = DosingLevel, y = SD.Lane.Deviation)) + geom_boxplot()
ggplot(outputs, aes(x = DosingLevel, y = Avg.Speed)) + geom_boxplot()

# Summary by Dosing Level
outputByDosing <- group_by(outputs, DosingLevel)
summarize(outputByDosing, 
          min = min(Avg.Speed, na.rm = TRUE), 
          Q1 = quantile(Avg.Speed, .25, na.rm = TRUE), 
          median = median(Avg.Speed, na.rm = TRUE), 
          Q3 = quantile(Avg.Speed, .75, na.rm = TRUE), 
          max = max(Avg.Speed, na.rm = TRUE))
          
summarize(outputByDosing, 
          min = min(Avg.Speed, na.rm = TRUE), 
          Q1 = quantile(Avg.Speed, .25, na.rm = TRUE), 
          median = median(Avg.Speed, na.rm = TRUE), 
          Q3 = quantile(Avg.Speed, .75, na.rm = TRUE), 
          max = max(Avg.Speed, na.rm = TRUE))
