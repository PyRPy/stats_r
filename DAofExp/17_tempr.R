# tempr.r, temperature experiment, Table 17.16 p662,
# and code from text p664
# three different type of thermostat at two different locations
tempr.data <- read.table("Data/temperature.txt", header=T) 
tempr.data <- within(tempr.data, {
   fTherm = factor(Therm); fSite = factor(Site); fSubj = factor(Subj) })
head(tempr.data, 3)

# Least squares anova, specifying random effects as error terms
options(contrasts = c("contr.sum", "contr.poly")) 
model1 <- aov(Time ~ fTherm + fSite + fTherm:fSite 
                   + Error(fSubj + fSubj:fSite), data=tempr.data)
summary(model1) 

# Means and contrasts: estimates, CIs, tests
library(lsmeans) 
lsmSite <- lsmeans(model1, ~ fSite) # Compute and save lsmeans
confint(lsmSite, level=0.95) # Display lsmeans and 95% CIs

# Pairwise comparison
summary(contrast(lsmSite, method="pairwise"), 
        infer=c(T,T), level=0.95, side="two-sided")

# Code from text p664
# Syntax for Tukey's method, though here we have only 2 levels:
summary(contrast(lsmSite, method="pairwise", adjust="tukey"), 
        infer=c(T,T), level=0.95, side="two-sided")

# Syntax for one or more contrasts specified in a list:
summary(contrast(lsmSite, list(Pairwise=c( 1, -1))),
        infer=c(T,T), level=0.95, side="two-sided")
