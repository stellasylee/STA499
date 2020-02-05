# Lane Departure Variable

library(dplyr)

# Artist Task ====
artist <- read.csv("H:\\NIDA\\final\\finalArtist.csv")
artistTimes <- read.csv ("H:\\NIDA\\final\\artistTimes.csv")
artist[,"Lane.Departure.Minor"] <- rep(-1,nrow(artist))
artist[,"Lane.Departure.Major"] <- rep(-1,nrow(artist))
artist[,"Lane.Departure.Severe"] <- rep(-1,nrow(artist))
artist[,"Init.Lane.Pos"] <- rep(0,nrow(artist))
artist <- artist[order(artist$ID, artist$DosingLevel, artist$eventNum), ]
artistTimes <- artistTimes[order(artistTimes$ID, artistTimes$DosingLevel, artistTimes$eventNum), ]
fileNames <- artistTimes$DaqName
index <- 1
for (i in 1:length(fileNames)){
  print(paste0(i, " ", fileNames[i]))
  if (i == 1 || fileNames[i-1] != fileNames[i]){
    file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  }
  start <- artistTimes$start[i]
  end <- artistTimes$end[i]
  duration <- (end - start)
  countMinor <- 0
  countMajor <- 0
  countSevere <- 0
  
  for (j in start:end){
    if (abs(file$SCC.Lane.Deviation.2[j]) >= 6){
      countSevere <- countSevere + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 4.5){
      countMajor <- countMajor + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 3){
      countMinor <- countMinor + 1
    }
  }
  artist[index, "Lane.Departure.Minor"] <- as.numeric(countMinor / duration * 100)
  artist[index, "Lane.Departure.Major"] <- as.numeric(countMajor / duration * 100)
  artist[index, "Lane.Departure.Severe"] <- as.numeric(countSevere / duration * 100)
  
  ## Also save initial lane dev (to adjust for in model)
  artist[index, "Init.Lane.Pos"] <- as.numeric(file$SCC.Lane.Deviation.2[start])
  
  countMinor <- 0
  countMajor <- 0
  countSevere <- 0
  
  for (j in (start - duration):start){
    if (abs(file$SCC.Lane.Deviation.2[j]) >= 6){
      countSevere <- countSevere + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 4.5){
      countMajor <- countMajor + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 3){
      countMinor <- countMinor + 1
    }
  }
  
  artist[(index+1), "Lane.Departure.Minor"] <- as.numeric(countMinor / duration * 100)
  artist[(index+1), "Lane.Departure.Major"] <- as.numeric(countMajor / duration * 100)
  artist[(index+1), "Lane.Departure.Severe"] <- as.numeric(countSevere / duration * 100)
  
  ## Also save initial lane dev (to adjust for in model)
  artist[(index +1), "Init.Lane.Pos"] <- as.numeric(file$SCC.Lane.Deviation.2[(start - duration)])
  
  
  index <- index + 2
}

write.csv(artist, "H:\\NIDA\\final\\finalFinalArtistLD.csv")

# Message Reading ====
message <- read.csv("H:\\NIDA\\final\\analysisMesBrake.csv")
messagetimes <- read.csv("H:\\NIDA\\final\\eventTimesMessage.csv")
message <- message[order(message$ID, message$DosingLevel, message$eventNum), ]
messagetimes <- messagetimes[order(messagetimes$ID, messagetimes$DosingLevel, messagetimes$eventNum), ]
message[,"Lane.Departure.Minor"] <- rep(-1,nrow(message))
message[,"Lane.Departure.Major"] <- rep(-1,nrow(message))
message[,"Lane.Departure.Severe"] <- rep(-1,nrow(message))
message["Init.Lane.Pos"] <- rep(NA,nrow(message))
fileNames <- messagetimes$DaqName
index <- 1
for (i in 1:length(fileNames)){
  print(paste0(i, " ", fileNames[i]))
  if (i == 1 || fileNames[i-1] != fileNames[i]){
    file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  }
  start <- messagetimes$start[i]
  end <- messagetimes$end[i]
  duration <- (end - start)
  countMinor <- 0
  countMajor <- 0
  countSevere <- 0
  
  for (j in start:end){
    if (abs(file$SCC.Lane.Deviation.2[j]) >= 6){
      countSevere <- countSevere + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 4.5){
      countMajor <- countMajor + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 3){
      countMinor <- countMinor + 1
    }
  }
  
  message[index, "Lane.Departure.Minor"] <- as.numeric(countMinor / duration * 100)
  message[index, "Lane.Departure.Major"] <- as.numeric(countMajor / duration * 100)
  message[index, "Lane.Departure.Severe"] <- as.numeric(countSevere / duration * 100)
  
  ## Also save initial lane dev (to adjust for in model)
  message[index, "Init.Lane.Pos"] <- as.numeric(file$SCC.Lane.Deviation.2[start])
  
  countMinor <- 0
  countMajor <- 0
  countSevere <- 0
  
  for (j in (start - duration):start){
    if (abs(file$SCC.Lane.Deviation.2[j]) >= 6){
      countSevere <- countSevere + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 4.5){
      countMajor <- countMajor + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 3){
      countMinor <- countMinor + 1
    }
  }
  
  message[(index+1), "Lane.Departure.Minor"] <- as.numeric(countMinor / duration * 100)
  message[(index+1), "Lane.Departure.Major"] <- as.numeric(countMajor / duration * 100)
  message[(index+1), "Lane.Departure.Severe"] <- as.numeric(countSevere / duration * 100)
  
  ## Also save initial lane dev (to adjust for in model)
  message[(index +1), "Init.Lane.Pos"] <- as.numeric(file$SCC.Lane.Deviation.2[(start - duration)])
  
  
  index <- index + 2
}

write.csv(message, "H:\\NIDA\\final\\finalFinalMessage.csv")

# Side Mirror Reading ====
mirror <- read.csv ("H:\\NIDA\\final\\analysisMirror.csv") # this already changed 299 frames to valid = 0 
mirror <- dplyr::filter (mirror, total <= 300)
mirror[,"Lane.Departure.Minor"] <- rep(-1,nrow(mirror))
mirror[,"Lane.Departure.Major"] <- rep(-1,nrow(mirror))
mirror[,"Lane.Departure.Severe"] <- rep(-1,nrow(mirror))
mirror[,"Init.Lane.Pos"] <- rep(0,nrow(mirror))
fileNames <- mirror$DaqName
i <- 1
while (i <= length(fileNames)){
  print(paste0(i, " ", fileNames[i]))
  if (i == 1 || fileNames[i-1] != fileNames[i]){
    file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  }
  
  start <- mirror$start[i]
  end <- mirror$end[i]
  duration <- mirror$total[i]
  countMinor <- 0
  countMajor <- 0
  countSevere <- 0
  
  for (j in start:end){
    if (abs(file$SCC.Lane.Deviation.2[j]) >= 6){
      countSevere <- countSevere + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 4.5){
      countMajor <- countMajor + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 3){
      countMinor <- countMinor + 1
    }
  }
  
  mirror[i, "Lane.Departure.Minor"] <- as.numeric(countMinor / duration * 100)
  mirror[i, "Lane.Departure.Major"] <- as.numeric(countMajor / duration * 100)
  mirror[i, "Lane.Departure.Severe"] <- as.numeric(countSevere / duration * 100)
  
  mirror[i, "Init.Lane.Pos"] <- as.numeric(file$SCC.Lane.Deviation.2[start])
  
  
  countMinor <- 0
  countMajor <- 0
  countSevere <- 0
  
  for (j in (start - duration):start){
    if (abs(file$SCC.Lane.Deviation.2[j]) >= 6){
      countSevere <- countSevere + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 4.5){
      countMajor <- countMajor + 1
    }else if (abs(file$SCC.Lane.Deviation.2[j]) >= 3){
      countMinor <- countMinor + 1
    }
  }
  
  mirror[(i+1), "Lane.Departure.Minor"] <- as.numeric(countMinor / duration * 100)
  mirror[(i+1), "Lane.Departure.Major"] <- as.numeric(countMajor / duration * 100)
  mirror[(i+1), "Lane.Departure.Severe"] <- as.numeric(countSevere / duration * 100)
  
  ## Also save initial lane dev (to adjust for in model)
  mirror[(i+1), "Init.Lane.Pos"] <- as.numeric(file$SCC.Lane.Deviation.2[(start - duration)])
  
  i <- i + 2
}

mirror$Lane.Departure[mirror$Lane.Departure.Severe > 100] <- 100
write.csv(mirror, "H:\\NIDA\\final\\finalFinalMirror.csv")
