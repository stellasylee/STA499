install.packages("readxl")
install.packages("xlsx")
install.packages(dplyr)
library(xlsx)
library(dplyr)
library(stringr)
library(readxl)

library(readxl)
#reading in auditory alert datafile

auditoryalert <- read_excel("H:/HapticProject/IMLTCombinedData.xlsx")




#reading in haptic alert data file file
hapticalert <- read_excel("H:/HapticProject/outputv27.xls")

#L=leidos, n=NADS, T=texas, U=UW

#filtering auditory alert file to include only IM event
IMauditoryalert <- filter(auditoryalert, IMOrLT == "IM")
#splitting auditory file into the regions

IMauditoryalert <- mutate(IMauditoryalert, region = Participant)

IMauditoryalert$region <- substring(IMauditoryalert$region, 0, 1)

#grouping the data by region

grouped_auditory <- group_by(IMauditoryalert, region)

#initial check whether the throttle release time is different for the different regions

lm1 <- lm(ThrottleRelease_time ~ region, data = grouped_auditory)

anova(lm1)

#therefore there is no site differnce for throttle release time


#look at whether ThrottleRelease_time is different for the different regions
