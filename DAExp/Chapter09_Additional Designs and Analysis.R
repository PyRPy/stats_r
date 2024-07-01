
# Chapter 9 Additional Designs and Analysis Topics for Factorial a --------
library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)

# Example: syrup loss during bottle filling -------------------------------
# nozzle, feedng speed, pressure as three main effects or factors
# three levels, three factors
head(Table9.1)
model <- aov(SyrupLoss ~ NozzleType * factor(Speed) * factor(Pressure), data=Table9.1)
print(summary(model))

# contour plot for reach nozzle
code.speed<-function(x) (x-120)/(140-120)
code.pressure<-function(x) (x-15)/(20-15)
df.reg<-data.frame(
  'NozzleType'=Table9.1$NozzleType,
  'Speed'=code.speed(Table9.1$Speed),
  'Pressure'=code.pressure(Table9.1$Pressure),
  'SyrupLoss'=Table9.1$SyrupLoss
)

# three linear models for each nozzle
model1<-lm(SyrupLoss~Speed*Pressure+I(Speed^2)+ I(Pressure^2),
           data=df.reg[df.reg$NozzleType==1,])
model2<-lm(SyrupLoss~Speed*Pressure+I(Speed^2)+ I(Pressure^2),
           data=df.reg[df.reg$NozzleType==2,])
model3<-lm(SyrupLoss~Speed*Pressure+I(Speed^2)+ I(Pressure^2),
           data=df.reg[df.reg$NozzleType==3,])
print(coef(model1))
print(coef(model2))
print(coef(model3))

# generate contour plots
x <- seq(from=100, to=140, length.out=100)
y <- seq(from=10, to=20, length.out=100)
df0 <- expand.grid('Speed'=code.speed(x), 'Pressure'=code.pressure(y))

z1 <- matrix(predict(model1, df0), ncol=100)
z2 <- matrix(predict(model2, df0), ncol=100)
z3 <- matrix(predict(model3, df0), ncol=100)

par(mfrow=c(2,2))
contour(x, y, z1, main='Nozzle Type 1')
contour(x, y, z2, main='Nozzle Type 2')
contour(x, y, z3, main='Nozzle Type 3')
plot.new() # This just uses up the last cell.
