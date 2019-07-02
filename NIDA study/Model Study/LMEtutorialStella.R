# http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf ----
# p.3
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex,pitch)

xmdl = lm(pitch ~ sex, my.df)
summary(xmdl)

# p.9
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age,pitch)
xmdl = lm(pitch ~ age, my.df)
summary(xmdl)

my.df$age.c = my.df$age - mean(my.df$age)
xmdl = lm(pitch ~ age.c, my.df)
summary(xmdl)
plot(fitted(xmdl),residuals(xmdl))
# p. 19
dfbeta(xmdl)

# http://www.bodowinter.com/tutorial/bw_LME_tutorial.pdf ----
library(lme4) # contains lmer() function = mixed model of function lm()
politeness = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
which(is.na(politeness)==T)
boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)
# both case, median line is lower for the polite than for informal condition
# following line will cause error because model doesn't have random effect (1| variable)
# lmer(frequency ~ attitude, data=politeness)

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
politeness.model

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)

# REML=FALSE argument is necessary for likelihood test
politeness.null = lmer(frequency ~ gender + (1|subject) + (1|scenario), data=politeness, REML=FALSE)
politeness.model = lmer(frequency ~ attitude +gender + (1|subject) + (1|scenario), data=politeness, REML=FALSE)
anova(politeness.null,politeness.model)
