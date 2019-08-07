# Lane Departure Variable

library(dplyr)

# Artist Task ====
artist <- read.csv("H:\\NIDA\\final\\finalArtist.csv")
artistTimes <- read.csv ("H:\\NIDA\\final\\artistTimes.csv")
artist[,"Lane.Departure"] <- rep(-1,nrow(artist))
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
  count <- 0
  for (j in start:end){
    if (abs(file$SCC.Lane.Deviation.2[j]) > 6){
      count <- count + 1
    }
  }
  artist[index, "Lane.Departure"] <- as.numeric(count / duration * 100)
  print(paste0(count, " ", (count / duration * 100)))
  count <- 0
  for (j in (start - duration):start){
    if (abs(file$SCC.Lane.Deviation.2[i]) > 6){
      count <- count + 1
    }
  }
  artist[(index+1), "Lane.Departure"] <- as.numeric(count / duration * 100)
  print((count / duration * 100))
  index <- index + 2
}

write.csv(artist, "H:\\NIDA\\final\\finalArtistLD.csv")

# Message Reading ====
message <- read.csv("H:\\NIDA\\final\\analysisMesBrake.csv")
messagetimes <- read.csv("H:\\NIDA\\final\\eventTimesMessage.csv")
message <- message[order(message$ID, message$DosingLevel, message$eventNum), ]
messagetimes <- messagetimes[order(messagetimes$ID, messagetimes$DosingLevel, messagetimes$eventNum), ]
message[,"Lane.Departure"] <- rep(-1,nrow(message))
fileNames <- messagetimes$DaqName
index <- 1
for (i in 1:length(fileNames)){
  print(paste0(i, " ", fileNames[i]))
  if (i == 1 || fileNames[i-1] != fileNames[i]){
    file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  }
  start <- messagetimes$start[i]
  end <- messagetimes$end[i]
  duration <- (end - start)
  count <- 0
  for (j in start:end){
    if (abs(file$SCC.Lane.Deviation.2[j]) > 6){
      count <- count + 1
    }
  }
  message[index, "Lane.Departure"] <- as.numeric(count / duration * 100)
  print(paste0(count, " ", (count / duration * 100)))
  count <- 0
  for (j in (start - duration):start){
    if (abs(file$SCC.Lane.Deviation.2[i]) > 6){
      count <- count + 1
    }
  }
  message[(index+1), "Lane.Departure"] <- as.numeric(count / duration * 100)
  print((count / duration * 100))
  index <- index + 2
}

write.csv(message, "H:\\NIDA\\final\\finalMessage.csv")

# Side Mirror Reading ====
mirror <- read.csv ("H:\\NIDA\\final\\analysisMirror.csv") # this already changed 299 frames to valid = 0 
mirror <- dplyr::filter (mirror, total <= 300)
mirror[,"Lane.Departure"] <- rep(-1,nrow(mirror))
fileNames <- mirror$DaqName
i <- 1
while (i <= length(fileNames)){
  print(paste0(i, " ", fileNames[i]))
  if (i == 1 || fileNames[i-1] != fileNames[i]){
    file <- read.csv(paste0("H:\\NIDA\\SideMirrorCSV\\", fileNames[i]))
  }
  
  start <- mirror$start[i]
  end <- mirror$end[i]
  duration <- mirror$total[i]
  count <- 0
  
  for (j in start:end){
    if (abs(file$SCC.Lane.Deviation.2[j]) > 6){
      count <- count + 1
    }
  }
  mirror[i, "Lane.Departure"] <- as.numeric(count / duration * 100)
  print(paste0("experiment done"))
  if (count != 0){
    print(paste0(count, " ", duration))
  }
  count <- 0
  for (j in (start - duration):start){
    if (abs(file$SCC.Lane.Deviation.2[i]) > 6){
      count <- count + 1
    }
  }
  mirror[(i+1), "Lane.Departure"] <- as.numeric(count / duration * 100)
  if (count != 0){
    print(paste0(count, " ", duration))
  }
  i <- i + 2
}

mirror$Lane.Departure[mirror$Lane.Departure > 100] <- 100
write.csv(mirror, "H:\\NIDA\\final\\finalMirror.csv")
