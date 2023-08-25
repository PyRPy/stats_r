# Likelihood and Bayesian inference and computation -----------------------

library(R2OpenBUGS)
library ("arm")
library(Matrix)
library(haven)
library(MCMCpack)

# Data preparation for the Chapter 18 -------------------------------------

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

# run the first section to get the data ready for the following sections
## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("ARM_Data/radon/cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

# 18.3 Bayesian inference for classical and multilevel regression ---------

## Complete pooling regression
lm.pooled <- lm (y ~ x)
display (lm.pooled)

## No pooling regression
lm.unpooled <- lm (y ~ x + factor(county)-1)
display (lm.unpooled)

## Multilevel model with no group-level predictors
M1 <- lmer (y ~ x + (1 | county))
display(M1)

## Multilevel model with group-level predictors
M2 <- lmer (y ~ x + u + (1 | county)) # u is not clear, not working
display(M2)

# 18.4 Gibbs sampler for multilevel linear models -------------------------

## Gibbs sampler in R
a.update <- function(){
  a.new <- rep (NA, J)
  for (j in 1:J){
    n.j <- sum (county==j)
    y.bar.j <- mean (y[county==j])
    a.hat.j <- ((n.j/sigma.y^2)*y.bar.j + (1/sigma.a^2)*mu.a)/
      (n.j/sigma.y^2 + 1/sigma.a^2)
    V.a.j <- 1/(n.j/sigma.y^2 + 1/sigma.a^2)
    a.new[j] <- rnorm (1, a.hat.j, sqrt(V.a.j))
  }
  return (a.new)
}
mu.a.update <- function(){
  mu.a.new <- rnorm (1, mean(a), sigma.a/sqrt(J))
  return (mu.a.new)
}
sigma.y.update <- function(){
  sigma.y.new <- sqrt(sum((y-a[county])^2)/rchisq(1,n-1))
  return (sigma.y.new)
}
sigma.a.update <- function(){
  sigma.a.new <- sqrt(sum((a-mu.a)^2)/rchisq(1,J-1))
  return (sigma.a.new)
}

n.chains <- 3
n.iter <- 1000
sims <- array (NA, c(n.iter, n.chains, J+3))
dimnames (sims) <- list (NULL, NULL, c (paste ("a[", 1:J, "]", sep=""), "mu.a",
                                        "sigma.y", "sigma.a"))

for (m in 1:n.chains){
  mu.a <- rnorm (1, mean(y), sd(y))
  sigma.y <- runif (1, 0, sd(y))
  sigma.a <- runif (1, 0, sd(y))
  for (t in 1:n.iter){
    a <- a.update ()
    mu.a <- mu.a.update ()
    sigma.y <- sigma.y.update ()
    sigma.a <- sigma.a.update ()
    sims[t,m,] <- c (a, mu.a, sigma.y, sigma.a)
  }
}

sims.bugs <- as.bugs.array (sims)
plot (sims.bugs)


