

# Read in the data
library("arm")

# 5.1 Logistic regression with a single predictor -------------------------


## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes

# The R codes & data files should be saved in the same directory for
# the source command to work

source("data_nes.R") # where data was cleaned; set the directory to be where this file is

yr <- 1992
ok <- nes.year==yr & presvote<3
vote <- presvote[ok] - 1
income <- data$income[ok]

# Estimation
fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
summary(fit.1)

# 5.2 Interpreting the logistic regression coefficients -------------------

## Evaluation at the mean

invlogit(-1.40 + 0.33*mean(income, na.rm=T))

# equivalently
fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
display (fit.1)
invlogit(coef(fit.1)[1] + coef(fit.1)[2]*mean(income, na.rm=T))

# -------------------------------------------------------------------------

wells <- read.table ("ARM_Data/wells.dat")
attach(wells)

## Histogram on distance (Figure 5.8)

hist (dist, breaks=seq(0,10+max(dist[!is.na(dist)]),10),
      xlab="Distance (in meters) to the nearest safe well",
      ylab="", main="", mgp=c(2,.5,0))

## Logistic regression with one predictor

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
summary(fit.1)

## Repeat the regression above with distance in 100-meter units

dist100 <- dist/100
fit.2 <- glm (switch ~ dist100, family=binomial(link="logit"))
summary(fit.2)

# 5.5 Logistic regression with interactions -------------------------------

fit.4 <- glm (switch ~ dist100 + arsenic + dist100:arsenic,
              family=binomial(link="logit"))
summary(fit.4)
## Centering the input variables

c.dist100 <- dist100 - mean (dist100)
c.arsenic <- arsenic - mean (arsenic)

## Refitting the model with centered inputs

fit.5 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic,
              family=binomial(link="logit"))
summary(fit.5)

# 5.6 Evaluating, checking, and comparing fitted logistic regressi --------

## Fitting the model
educ4 <- educ/4
c.educ4 <- educ4 - mean(educ4)
fit.8 <- glm (switch ~ c.dist100 + c.arsenic + c.educ4 + c.dist100:c.arsenic +
                c.dist100:c.educ4 + c.arsenic:c.educ4, family=binomial(link="logit"))
display (fit.8)

## Log transformation

log.arsenic <- log (arsenic)
c.log.arsenic <- log.arsenic - mean (log.arsenic)

fit.9 <- glm (switch ~ c.dist100 + c.log.arsenic + c.educ4 +
                c.dist100:c.log.arsenic + c.dist100:c.educ4 + c.log.arsenic:c.educ4,
              family=binomial(link="logit"))
display (fit.9)

fit.9a <- glm (switch ~ dist100 + log.arsenic + educ4 +
                 dist100:log.arsenic + dist100:educ4 + log.arsenic:educ4,
               family=binomial(link="logit"))

pred.9 <- fit.9$fitted.values

# for modell fit.9
error.rate <- mean((pred.9>0.5 & switch==0) | (pred.9<0.5 & switch==1))

# 5.7 Average predictive comparisons on the probability scale -------------

## Fitting the model

fit.10 <- glm (switch ~ dist100 + arsenic + educ4,
               family=binomial(link="logit"))
display (fit.10)

## Avg predictive differences

b <- coef(fit.10)

# for distance to nearest safe well

hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*arsenic + b[4]*educ4) -
  invlogit (b[1] + b[2]*lo + b[3]*arsenic + b[4]*educ4)
print (mean(delta))

#  for arsenic level

hi <- 1.0
lo <- 0.5
delta <- invlogit (b[1] + b[2]*dist100 + b[3]*hi + b[4]*educ4) -
  invlogit (b[1] + b[2]*dist100 + b[3]*lo + b[4]*educ4)
print (mean(delta))

# for education

hi <- 3
lo <- 0
delta <- invlogit (b[1]+b[2]*dist100+b[3]*arsenic+b[4]*hi) -
  invlogit (b[1]+b[2]*dist100+b[3]*arsenic+b[4]*lo)
print (mean(delta))

## Avg predictive comparisons with interactions

fit.11 <- glm (switch ~ dist100 + arsenic + educ4 + dist100:arsenic,
               family=binomial(link="logit"))
display (fit.11)

# for distance

b <- coef (fit.11)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*arsenic + b[4]*educ4 +
                     b[5]*hi*arsenic) -
  invlogit (b[1] + b[2]*lo + b[3]*arsenic + b[4]*educ4 +
              b[5]*lo*arsenic)
print (mean(delta))

# 5.8 Identifiability and separation --------------------------------------

## Generating the variables

x <- rnorm(60, mean =1, sd = 2)
y <- ifelse(x<2,0,1)

## Fit the model

fit.0 <- glm (y ~ x, family=binomial(link="logit"))

## Plot

plot (x, y, xlab="x", ylab="y", xlim=c(-6,6), pch=20)
curve (invlogit (coef(fit.0)[1] + coef(fit.0)[2]*x), add=TRUE)

