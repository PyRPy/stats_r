
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

# check model's residues
qqnorm(resid(model))
qqline(resid(model), col='orange')

# check residue vs predicts and variances of residues
plot(resid(model), main='Residual vs. Run Order')
plot(x=predict(model), y=resid(model), main='Predicted vs. Residual')

# Bartlett’s test for checking equality of variances
bartlett.test(Observation ~ Power, data=Table3.1)


# estimating flood flow frequency
# check data structure and variable types
str(Example3.5)
model <- aov(Observation ~ EstimationMethod, data=Example3.5)
summary(model)

# plot of predicted-versus-residual
plot(predict(model), resid(model), main='Predicted vs. Residual')

# use square root transformation on y or observation
y <- sqrt(Example3.5$Observation)
model <- aov(y ~ EstimationMethod, data=Example3.5)
summary(model)

plot(predict(model), resid(model), main='Predicted vs. Residual')
# after transformation, no obvious pattern observed.


# Regression Model --------------------------------------------------------

x <- as.numeric(as.character(Table3.1$Power))
linear.model <- lm(Observation ~ x, data=Table3.1)
quadratic.model <- lm(Observation ~ x + I(x^2), data=Table3.1)

plot(x, Table3.1$Observation, main='Linear Model')
abline(a=coef(linear.model)[1], b=coef(linear.model)[2], col='orange')
plot(x, Table3.1$Observation, main='Quadratic Model')
u <- seq(from=160, to=220, length.out=100)
v <- cbind(rep(1, length(u)), u, u^2) %*% coef(quadratic.model)
lines(u, v, col='orange')


# Contrasts ---------------------------------------------------------------
# read textbook, it has good explanation and examples
model <- aov(Observation ~ Power, data=Table3.1)
summary(model)

library(emmeans)
emmodel <- emmeans(model, ~ Power)
contrast(emmodel, list(
  'C1'=c(1, -1, 0, 0),
  'C2'=c(1, 1, -1, -1),
  'C3'=c(0, 0, 1, -1)
))

# Tukey adjustment for all pairs comparison
pairs(emmodel, adjust="tukey")
