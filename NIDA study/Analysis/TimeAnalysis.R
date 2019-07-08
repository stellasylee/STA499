library(stringr)
library(tidyverse)
library ("dplyr")

eventTimes <- read.csv("H:\\NIDA\\eventTimesWith3.csv")
eventTimesByInstance <- group_by(eventTimes, eventNum)
summarize(eventTimesByInstance, 
          min = min(total, na.rm = TRUE), 
          Q1 = quantile(total, .25, na.rm = TRUE), 
          median = median(total, na.rm = TRUE), 
          Q3 = quantile(total, .75, na.rm = TRUE), 
          max = max(total, na.rm = TRUE))

eventTimesByDosing <- group_by(eventTimes, DosingLevel)
summarize(eventTimesByDosing, 
          min = min(total, na.rm = TRUE), 
          Q1 = quantile(total, .25, na.rm = TRUE), 
          median = median(total, na.rm = TRUE), 
          Q3 = quantile(total, .75, na.rm = TRUE), 
          max = max(total, na.rm = TRUE))


eventTimes$eventNum <- as.factor (eventTimes$eventNum)

totalTimeByInstance <- ggplot(eventTimes, aes(x = eventNum, y = total)) + geom_boxplot()
totalTimeByDosing <- ggplot(eventTimes, aes(x = DosingLevel, y = total)) + geom_boxplot()
