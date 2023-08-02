
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon
library ("arm")
library(Matrix)
# 12.2 Partial pooling with no predictors ---------------------------------

srrs2 <- read.table ("ARM_Data/radon/srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"
radon <- srrs2$activity[mn]
log.radon <- log (ifelse (radon==0, .1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# get county index variable
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

# no predictors
ybarbar = mean(y)

sample.size <- as.vector (table (county))
sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))
cty.mns = tapply(y,county,mean)
cty.vars = tapply(y,county,var)
cty.sds = mean(sqrt(cty.vars[!is.na(cty.vars)]))/sqrt(sample.size)
cty.sds.sep = sqrt(tapply(y,county,var)/sample.size)


# 12.3 Partial pooling with predictors ------------------------------------
## Complete pooling regression
lm.pooled <- lm (y ~ x)
display (lm.pooled)

## No pooling regression
lm.unpooled <- lm (y ~ x + factor(county) -1)
display (lm.unpooled)


# 12.4 Quickly fitting multilevel models in R -----------------------------
## Varying-intercept model w/ no predictors
M0 <- lmer (y ~ 1 + (1 | county))
display (M0)

## Including x as a predictor
M1 <- lmer (y ~ x + (1 | county))
display (M1)

# estimated regression coefficicents
coef (M1)

# fixed and random effects
fixef (M1)
ranef (M1)

# uncertainties in the estimated coefficients
se.fixef (M1)
se.ranef (M1)

# 95% CI for the slope
fixef(M1)["x"] + c(-2,2)*se.fixef(M1)["x"]
#or
fixef(M1)[2] + c(-2,2)*se.fixef(M1)[2]

# 95% CI for the intercept in county 26
coef(M1)$county[26,1] + c(-2,2)*se.ranef(M1)$county[26]

# 95% CI for the error in the intercept in county 26
as.matrix(ranef(M1)$county)[26] + c(-2,2)*se.ranef(M1)$county[26]


# 12.6 Group-level predictors ---------------------------------------------

## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("ARM_Data/radon/cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

## Varying-intercept model w/ group-level predictors
u.full <- u[county]
M2 <- lmer (y ~ x + u.full + (1 | county))
display (M2)

coef (M2)
fixef (M2)
ranef (M2)


# 12.8 Predictions for new observations and new groups --------------------

## Prediction for a new observation in a new group (new house in county 26
## with x=1)
M2 <- lmer (y ~ x + u.full + (1 | county))
a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3]*u + ranef(M2)$county
b.hat.M2 <- fixef(M2)[2]

x.tilde <- 1
sigma.y.hat <- sigma.hat(M2)$sigma$data
coef.hat <- as.matrix (coef(M2)$county)[26,]
y.tilde <- rnorm (1, coef.hat %*% c(1, x.tilde, u[26]), sigma.y.hat)
n.sims <- 1000
y.tilde <- rnorm (n.sims, coef.hat %*% c(1, x.tilde, u[26]), sigma.y.hat)

quantile (y.tilde, c(.25, .5, .75))

unlogged <- exp(y.tilde)
mean(unlogged)

## Prediction for a new observation in an existing group (new house in
## a new county)
u.tilde <- mean (u)
g.0.hat <- fixef(M2)["(Intercept)"]
g.1.hat <- fixef(M2)["u.full"]
sigma.a.hat <- sigma.hat(M2)$sigma$county

a.tilde <- rnorm (n.sims, g.0.hat + g.1.hat*u.tilde, sigma.a.hat)
y.tilde <- rnorm (n.sims, a.tilde + b.hat.M2*x.tilde, sigma.y.hat)

quantile (y.tilde, c(.25,.5,.75))

exp (quantile (y.tilde, c(.25,.5,.75)))

## Nonlinear predictions
y.tilde.basement <- rnorm (n.sims, a.hat.M2[26,], sigma.y.hat)
print (y.tilde.basement)

y.tilde.nobasement <- rnorm (n.sims, a.hat.M2[26,] + b.hat.M2, sigma.y.hat)
print (y.tilde.nobasement)

mean.radon.basement <- mean (exp (y.tilde.basement))
print (mean.radon.basement)

mean.radon.nobasement <- mean (exp (y.tilde.nobasement))
print (mean.radon.nobasement)

mean.radon <- .9*mean.radon.basement + .1*mean.radon.basement
print (mean.radon)
