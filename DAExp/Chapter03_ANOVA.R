
# Chapter 3 One-Way ANOVA -------------------------------------------------

# https://github.com/ehassler/MontgomeryDAE/tree/master

library(MASS)
library(MontgomeryDAE)
head(Table3.1)
is.factor(Table3.1$Power)
is.character(Table3.1$Power)



# ANOVA one way -----------------------------------------------------------
# data of RF power to the etching depth

summary(aov(Observation ~ Power, data = Table3.1))


# Linear regression model -------------------------------------------------

model <- lm(Observation ~ as.factor(Power), data=Table3.1)
summary(model)
