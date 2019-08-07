mirror <- read.csv ("H:\\NIDA\\analysisMirror.csv") # this already changed 299 frames to valid = 0 

m2 <- filter(mirror, Experiment == 1)
m2$Dose_ID <- paste(m2$ID,m2$DosingLevel)
m3 <- aggregate(. ~ Dose_ID, m2, FUN = head, 1)

m3$DosingN <- factor(as.character(m3$DosingLevel), labels =  c(2,1,4,3,6,5))


pdose <- ggplot()  +
  geom_line(data = m3, aes(x = as.numeric(as.character(DosingN)), y = THC, group = factor(ID), color = factor(ID)), lty = 2) + 
  geom_point(data = m3, aes(x = as.numeric(as.character(DosingN)), y = THC, group = factor(ID)), cex = .9) + theme_classic() +
  scale_x_continuous(name = "", breaks = 1:6, labels = c("Placebo/\nPlacebo", "Placebo/\nAlcohol", "Low THC/\nPlacebo", 
                                                         "Low THC/\nAlcohol", "High THC/\nPlacebo", "High THC/\nAlcohol")) + 
  labs(y = expression(Blood ~ THC ~ (mu*g/L)), col = "Subject ID")


png("H:\\Figure1.png", h=5.5, w=7, units = 'in', res = 300)
pdose
dev.off()