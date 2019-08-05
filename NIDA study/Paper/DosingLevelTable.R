# Creating Table for paper
library(dplyr)

# Mirror Task ====
mirror <- read.csv ("H:\\NIDA\\analysisMirror.csv") # this already changed 299 frames to valid = 0 
mirror <- dplyr::filter (mirror, total <= 300)
normalMirror <- filter(mirror, !(ID == 123 & Visit == 1))
normalMirrorExp <- filter(normalMirror, Experiment == 1)
normalMirrorExp$LogStreams.5

# Urban Segment ----
mirrorUrban <- filter (normalMirrorExp, normalMirrorExp$LogStreams.5 <= 14)
prop.table(table(mirrorUrban$DosingLevel, mirrorUrban$valid), 1)
prop.table(table(mirrorUrban$valid))

mean(mirrorUrban$THC)
mean(mirrorUrban$BAC)
mirrorByDosingLevel <- group_by(mirrorUrban, DosingLevel)
summarize(mirrorByDosingLevel, 
          THC = mean(THC, na.rm = TRUE), 
          BAC = mean(BAC, na.rm = TRUE))

complete <- filter(mirrorUrban, valid == 1)
complete$time <- complete$total/60
summarize(complete, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))
mirrorByDosingLevel <- group_by(complete, DosingLevel)
summarize(mirrorByDosingLevel, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))

# Interstate Segment ---
mirrorInter <- filter (normalMirrorExp, normalMirrorExp$LogStreams.5 %in% c(21,22,23))
prop.table(table(mirrorInter$DosingLevel, mirrorInter$valid), 1)
prop.table(table(mirrorInter$valid))

mean(mirrorInter$THC)
mean(mirrorInter$BAC)
mirrorByDosingLevel <- group_by(mirrorInter, DosingLevel)
summarize(mirrorByDosingLevel, 
          THC = mean(THC, na.rm = TRUE), 
          BAC = mean(BAC, na.rm = TRUE))

complete <- filter(mirrorInter, valid == 1)
complete$time <- complete$total/60
summarize(complete, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))
mirrorByDosingLevel <- group_by(complete, DosingLevel)
summarize(mirrorByDosingLevel, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))

# Rural Segment ----
mirrorRural <- filter (normalMirrorExp, normalMirrorExp$LogStreams.5 %in% c(31,32,33))
prop.table(table(mirrorRural$DosingLevel, mirrorRural$valid), 1)
prop.table(table(mirrorRural$valid))

mean(mirrorRural$THC)
mean(mirrorRural$BAC)
mirrorByDosingLevel <- group_by(mirrorRural, DosingLevel)
summarize(mirrorByDosingLevel, 
          THC = mean(THC, na.rm = TRUE), 
          BAC = mean(BAC, na.rm = TRUE))

complete <- filter(mirrorRural, valid == 1)
complete$time <- complete$total/60
summarize(complete, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))

mirrorByDosingLevel <- group_by(complete, DosingLevel)
summarize(mirrorByDosingLevel, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))
summarize(mirrorRural, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))

# Artist Task ====
artist <- read.csv("H:\\NIDA\\finalArtist.csv")
artistExp <- filter (artist, Experiment == 1)

prop.table(table(artistExp$DosingLevel, artistExp$valid), 1)
prop.table(table(artistExp$valid))

prop.table(table(artistExp$DosingLevel, artistExp$incorrect), 1)
prop.table(table(artistExp$incorrect))

mean(artistExp$THC)
mean(artistExp$BAC)
artistByDosingLevel <- group_by(artistExp, DosingLevel)
summarize(artistByDosingLevel, 
          THC = mean(THC, na.rm = TRUE), 
          BAC = mean(BAC, na.rm = TRUE))

complete <- filter(artistExp, valid == 1)
complete$time <- complete$total/60
summarize(complete, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))
artistByDosingLevel <- group_by(complete, DosingLevel)
summarize(artistByDosingLevel, 
          time = mean(time, na.rm = TRUE),
          SDLD = mean(SD.Lane.Deviation, na.rm = TRUE), 
          Speed = mean(Avg.Speed, na.rm = TRUE),
          SDS = mean(SD.Speed, na.rm = TRUE))
