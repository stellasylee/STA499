# Lane Departure Variable

library(dplyr)

# Artist Task ====
artist <- read.csv("H:\\NIDA\\final\\finalArtist.csv")
artistTimes <- read.csv ("H:\\NIDA\\final\\artistTimes.csv")
artist[,"Lane.Departure"] <- rep(-1,nrow(artist))
fileNames <- artistTimes$DaqName

for (i in 1:length(fileNames)){
  print(fileNames[i])
  if (i != 1 && fileNames[i-1] != fileNames[i]){
    file <- read.csv(paste0("H:\\NIDA\\ReducedCSV\\", fileNames[i]))
  }
  start <- artistTimes$start[i]
  end <- artistTimes$end[i]
  duration <- (end - start)
  count <- 0
  for (j in start:end){ # experiment group
    if (abs(file$SCC.Lane.Deviation.2[j]) > 6){
      count <- count + 1
    }
  }
  artist[(2*i-1), "Lane.Departure"] <- as.numeric(count / duration * 100)
  count <- 0
  for (j in (start - duration):start){ # control group
    if (abs(file$SCC.Lane.Deviation.2[i]) > 6){
      count <- count + 1
    }
  }
  artist[(2*i), "Lane.Departure"] <- as.numeric(count / duration * 100)
}
