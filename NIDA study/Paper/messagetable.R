
library(stringr) # String manipulation
library(readxl) 
library(dplyr)
library(lme4)
library(lmerTest)
library(plotly)

#reading in message task files
#eventTimesMessage <- read.csv("H:\\CannabisStudy\\message\\eventTimesMessage.csv")
analysisMes <- read.csv("H:\\CannabisStudy\\message\\analysisMesBrake.csv")

#filtering out and creating separate dataframes for experimental and control groups

MessageExperiment <- dplyr::filter(analysisMes, Experiment == "1" )
MessageControl <- dplyr::filter(analysisMes, Experiment == "0")

#creating road segments

urbanmessage <- filter(MessageExperiment, LogStreams.5 == "11" | LogStreams.5 == "12" | LogStreams.5 == "13"  )

ruralmessage <- filter(MessageExperiment, LogStreams.5 == "32" | LogStreams.5 == "33")

interstatemessage <- filter(MessageExperiment, LogStreams.5 == "22")


#grouping by dosing level
grouped_urbanmes <- group_by(urbanmessage, DosingLevel)

grouped_ruralmes <- group_by(ruralmessage, DosingLevel)

grouped_interstatemes <- group_by(interstatemessage, DosingLevel)

#finding the summaries
summarize(grouped_urbanmes, mean(THC), mean(BAC))

summarize(grouped_ruralmes, mean(THC), mean(BAC))

summarize(grouped_interstatemes, mean(THC), mean(BAC))



#sorting these dataframes to ensure that the the rows in both of them align

MessageExperiment <- arrange(MessageExperiment, desc(DaqName, eventNum))
MessageControl <- arrange(MessageControl, desc(DaqName, eventNum))

MessageExperiment <- mutate(MessageExperiment, SD.Lane.Diff = MessageExperiment$SD.Lane.Deviation - MessageControl$SD.Lane.Deviation,
                            Avg.Speed.Diff = MessageExperiment$Avg.Speed - MessageControl$Avg.Speed, 
                            SD.Speed.Diff = MessageExperiment$Sd.Speed - MessageControl$Sd.Speed,
                            Break.Diff = MessageExperiment$Max.Brake - MessageControl$Max.Brake)

#urban message and grouped urban message predictor means

summarize(urbanmessage, 
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(Sd.Speed, na.rm = TRUE))

summarize(grouped_urbanmes, 
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(Sd.Speed, na.rm = TRUE))

#rural message and grouped rural message predictor means

summarize(ruralmessage, 
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(Sd.Speed, na.rm = TRUE))

summarize(grouped_ruralmes, 
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(Sd.Speed, na.rm = TRUE))

#interstate message and grouped interstate message predictor means

summarize(interstatemessage, 
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(Sd.Speed, na.rm = TRUE))

summarize(grouped_interstatemes, 
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(Sd.Speed, na.rm = TRUE))
