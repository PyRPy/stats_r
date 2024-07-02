
# Chapter 10 Fitting Regression Models ------------------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)

# set the contrast similar to the textbook
options(contrasts=c(unordered='contr.sum', ordered='contr.poly'))

# relationship of reaction temperature and catalyst feed rate to
# the viscosity of a polymer

head(Table10.2)
X <- model.matrix(~ReactionTemperature + CatalystFeedRate, data=Table10.2)
print(X)

y<-Table10.2$Viscosity
print(y)

# use matrix operations
beta.hat<-solve(t(X)%*%X,t(X)%*%y)
print(beta.hat)

# use linear model in R directly
model<-lm(Viscosity~ReactionTemperature+CatalystFeedRate, data=Table10.2)
print(summary(model))

# check the residues
Table10.3<-data.frame(
  'y'=Table10.2$Viscosity,
  'Predicted'=predict(model),
  'Residual'=resid(model),
  'hii'=hatvalues(model),
  'StudentizedResids'=rstudent(model),
  'CooksD'=cooks.distance(model)
)

# hypothesis test on beta2
model01<-lm(Viscosity~ReactionTemperature,data=Table10.2)
# add catalyst feedrate as the second factor
model012<-lm(Viscosity~ReactionTemperature+CatalystFeedRate, data=Table10.2)
print(anova(model01,model012))

# confidence interval on the regression coefficients
model <- lm(Viscosity ~ ReactionTemperature + CatalystFeedRate, data=Table10.2)
confint(model)

# prediction interval
x1 <- seq(from=80, to=100, length.out=100)
x2 <- rep(10.5, 100)
y <-predict(model, newdata=data.frame('ReactionTemperature'=x1,
                                      'CatalystFeedRate'=x2), interval="confidence")
plot(x1, y[,1], type='l',
     ylim=c(2248, 2434),
     main='Prediction Interval',
     xlab='Reaction Temperature',
     ylab='Viscosity')
lines(x1, y[,2], lty=2)
lines(x1, y[,3], lty=2)
