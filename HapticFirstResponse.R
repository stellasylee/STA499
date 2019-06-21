## Filter 40Hz Intetensity Level Participants
# Get participant number of 40Hz
group40 <- rand[which(rand$`AlertLevel` == 2),]$'Participant#' 

# Get file names corresponding to participant number
file40 <- filter(disp, substr(disp$'DaqPath', 2, 5) %in% group40) 
file40 <- filter(file40, substr(file40$'DaqPath', nchar(file40$'DaqPath') -1, nchar(file40$'DaqPath')) == "MN")$'DaqName'

## For each participant, we will get the participant first response
## find haptic alert start time
findAlert <- function (data) {
  haptic <- which(rownames(data$elemDataI) == 'SCC.Haptic.Alert')
  h <- data$elemDataI[[haptic]][,1]
  for (alert in 1: 13349){
    if (h[alert] != 0){
      alert <- numeric(alert)
      break
    }
  }
  return (alert)
}

for (i in (1:30)){
  test <- readMat(paste0("/Users/seoyeon/Documents/2019MAP/MatFiles/",file40[i]))
  start <- findAlert (test)
}
