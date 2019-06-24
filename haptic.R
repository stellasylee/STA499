library ("readxl")
library ("dplyr")

# Upload Data ----
haptic <- read_excel("H:\\haptic\\output_v27.xls")
## V2V Safety Systems Left Turn and Intersection Movement Assist events --> auditory
auditory <- read_excel("H:\\haptic\\IMLTCombinedData.xlsx")
# filter IMA events from auditory data
auditory <- filter (auditory, IMOrLT == "IM")

# Site difference ----
# Auditory alert data was collected in 4 site: 
# NADS, Texas A&M Transportation Institute (TTI), University of Washington (UW), and Leidos
## create variables indicating sites
## L -> Leidos N-> NADS T -> Texas U -> UW
auditory$sites <- substring(auditory$Participant, 1, 1) 
auditoryBySite <- group_by(auditory, sites)

# Outcome of Crash Event----
## Crash Frequency and rate
## Collision_TTC = 0 if there was collision
auditoryBySite %>% tally()
auditoryBySite %>% tally(Collision_TTC == 0) # Crash count
aud.mod1 = lm(Collision_TTC ~ sites, data = auditory)
summary(aud.mod1)
anova(aud.mod1)
### Analysis of Variance Table p-value = 0.06384

# Driver Performance----
## throttle release reation time
## maximum deceleration
aud.maxDeceleration = lm(MaxDeceleration ~ sites, data = auditory)
summary(aud.maxDeceleration)
anova(aud.maxDeceleration)
### Analysis of Variance Table: 
### F-value = 0.9634 p-value = 0.4109 

