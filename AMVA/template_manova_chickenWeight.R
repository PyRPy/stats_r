# MANOVA on chicken weight data -------------------------------------------
# https://www.mohanwugupta.com/post/mancova/mancova/

library(MASS)
library(dplyr)
data("ChickWeight")

summary(ChickWeight)

#Reshape data to wide format and keep key variables
chick = reshape(ChickWeight, idvar = "Chick", timevar = "Time", direction = "wide") %>% 
  select("weight.0", "weight.2", "weight.4", "Diet.0")

#Ensure Diet is a factor for the model
chick$Diet.0 = factor(chick$Diet.0, label = c("1", "2", "3", "4"))

#combine the two dependent variables so we can run them as one model instead of two (changes df)
outcome = cbind(chick$weight.2, chick$weight.4)

#MANCOVA model
m.mancova = manova(outcome ~ Diet.0 * weight.0, data = chick)
summary(m.mancova, test = "Wilks", type = "III")

aov(weight.0 ~ Diet.0, data = chick)

#install.packages("psych")
library(psych)
#describeBy(chick, chick$Diet.0)

model1 = aov(weight.2 ~ weight.0 + Diet.0, data = chick)
summary(model1, type = "III")

#install.packages("effects")
library(effects)

adjmean1 = effect("Diet.0", model1, se=TRUE)
summary(adjmean1)
adjmean1$se

model2 = aov(weight.4 ~ weight.0 + Diet.0, data = chick)
summary(model2, type = "III")

adjmean2 = effect("Diet.0", model2, se=TRUE)
summary(adjmean2)
adjmean2$se
