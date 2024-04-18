
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


# Determining Sample Size -------------------------------------------------

library(pwr)
w <- c(575, 600, 650, 675)
a <- length(w)
sigma <- 25
f <- sqrt((1/a)* sum((w - mean(w))^2)) / sigma # effective sample size

pwr.anova.test(k=4, f=f, n=3, sig.level=0.01)

# power curve
n <- seq(from=2, to=8, length.out=50)
y <- pwr.anova.test(k=4, f=f, sig.level=0.01, n=n)$power
plot(n, y, type='l',
     main='Power for Example 3.1',
     xlab='Sample Size',
     ylab='Power')

# 90% power would require a sample size of 4 at each group (16 overall)
pwr.anova.test(k=4, f=f, sig.level=0.01, power=0.9)

# Chocolate Consumption and antioxidant regarding human health
boxplot(Observation ~ Factor, data=Table3.12)

# general significance test
summary(aov(Observation ~ Factor, data=Table3.12))

# individual effects we use a linear model without an intercept
means.model <- lm(Observation ~ Factor - 1, data=Table3.12)
print(summary(means.model))

# confidence intervals for each chocolate level
confint(means.model)

# all pairwise comparisons
library(emmeans)
pairwise.model <- pairs(emmeans(aov(Observation ~ Factor,
                                    data=Table3.12), ~Factor), adjust='none')
print(pairwise.model)

# point estimates with confidence intervals for difference
confint(pairwise.model)


# Random Effects Model ----------------------------------------------------
# model the looms as a population
model <- aov(Strength ~ Error(Looms), data=Example3.10)
summary(model)

# random error model
library(lme4)
library(lmerTest)
model <- lmer(
  Strength ~ (1|Looms),
  data=Example3.10,
  REML=TRUE
)
print(summary(model))

print(rand(model))

# CI for variance components
confint(model, oldNames=FALSE)


# non-parametric Kruskal-Wallis test --------------------------------------

kruskal.test(Observation ~ Power, data=Table3.1)

