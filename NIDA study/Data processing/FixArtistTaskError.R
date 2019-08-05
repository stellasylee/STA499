library(stringr)
library(tidyverse)
library ("readxl")
library ("dplyr")

artist <- read.csv("H:\\NIDA\\analysisArtistNoEngage.csv")
artist <- artist[, 1:8]

times <- read.csv("H:\\NIDA\\Artist\\artistTimes.csv")
times2 <- times[,-(5:7)] %>% slice(rep(1:n(), each = 2))

artist <- artist[order(artist$ID, artist$DosingLevel, artist$eventNum), ] 
times2 <- times2[order(times2$ID, times2$DosingLevel, times2$eventNum), ] 

temp <- cbind(artist, times2[!names(times2) %in% names(artist)])

write.csv(temp, "H:\\NIDA\\finalArtist.csv")
