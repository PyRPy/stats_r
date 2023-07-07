## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/child.iq

# 3.1 One predictor -------------------------------------------------------

library("arm")
library(haven) # read .dta files
kidiq <- read_dta("ARM_Data/child.iq/kidiq.dta")
head(kidiq)

fit.0 <- lm (kid_score ~ mom_hs, data = kidiq)
display(fit.0)


# 3.2 Multiple predictors -------------------------------------------------

fit.2 <- lm (kid_score ~ mom_hs + mom_iq, data = kidiq)
summary(fit.2)


# 3.3 Interactions --------------------------------------------------------

fit <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data = kidiq)
display (fit)

# 3.4 Statistical inference -----------------------------------------------

fit.3 <- lm (kid_score ~ mom_hs + mom_iq)
summary(fit.3)


# 3.5 Graphical displays of data and fitted model -------------------------

## Regression line as a function of one input variable
fit.2 <- lm (kid_score ~ mom_iq, data = kidiq)
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.2)[1] + coef(fit.2)[2]*x, add=TRUE)

## model with no interaction
fit.3 <- lm (kid_score ~ mom_hs + mom_iq, data=kidiq)
colors <- ifelse (mom_hs==1, "black", "gray")
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
      col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.3), add=TRUE, col="gray")

## model with interaction
fit.4 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidiq)
colors <- ifelse (mom_hs==1, "black", "gray")
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
      col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.4), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.4), add=TRUE, col="gray")


# 3.6 Assumptions and diagnostics -----------------------------------------

## Fit the model
fit.2 <- lm (kid_score ~ mom_iq, data = kidiq)
resid <- fit.2$residuals
sd.resid <- sd(resid)

# Figure 3.12
plot (mom_iq, resid, xlab="Mother IQ score", ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)


# 3.7 Prediction and validation -------------------------------------------

## Model fit and prediction
fit.3 <- lm (kid_score ~ mom_hs + mom_iq, data = kidiq)
x.new <- data.frame (mom_hs=1, mom_iq=100)
predict (fit.3, x.new, interval="prediction", level=0.95)
