
# Chapter 11 Response Surface Methods and Designs -------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)


# Reaction time and temperature on product yield --------------------------

library(rsm)
head(Table11.1)

df <- Table11.1
# convert natural units to coded
cf <- coded.data(df, x1 ~ (Time- 35) / 5, x2 ~ (Temperature- 155) /5)
model <- rsm(Yield ~ FO(x1, x2), data=cf) # first order, lack of fit, not sig
summary(model)

# add two interaction terms
model<-rsm(Yield~FO(x1,x2)+TWI(x1,x2),data=cf) # lack of fit, not sig
summary(model)

# data table 11.6
head(Table11.6)
cf<-coded.data(Table11.6,x1~(Time-85)/5,x2~(Temperature - 175)/5)
model<-rsm(Yield~FO(x1,x2),data=cf) # lack of fit, significant
summary(model)$lof

# run the second order model
model<-rsm(Yield~SO(x1,x2),data=cf) # lack of fit, not sig
summary(model)$lof

#  response surface plots
contour(model, ~ x1 + x2, image = TRUE)
