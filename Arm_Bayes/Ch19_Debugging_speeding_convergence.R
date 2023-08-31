
# Debugging and speeding convergence --------------------------------------

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
n.county <- J
# run the first section to get the data ready for the following sections
## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("ARM_Data/radon/cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

## Redundant mean parameters for a simple nested model

model1 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- mu + eta[county[i]]
  }
  mu ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:n.county){
    eta[j] ~ dnorm (0, tau.eta)
  }
  tau.eta <- pow(sigma.eta, -2)
  sigma.eta ~ dunif (0, 100)
}

radon.data <- list ("n", "y", "n.county", "county")

radon.inits <- function(){
  list (mu=rnorm(1), mu.eta=rnorm(1), eta=rnorm(J), sigma.y=runif(1),
        sigma.eta=runif(1))
}

radon.parameters <- c ("mu", "eta", "sigma.y", "sigma.eta")
fit.1 <- bugs (radon.data, radon.inits, radon.parameters, model1, n.chains=3,
               n.iter=100, debug=TRUE )
plot(fit.1)

# use redundant parameters to speed up convergence
model2 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- mu + eta[county[i]]
  }
  mu ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  mu.adj <- mu + mean(eta[])
  for (j in 1:n.county){
    eta[j] ~ dnorm(mu.eta, tau.eta)
    eta.adj[j] <- eta[j] - mean(eta[])
  }
  mu.eta ~ dnorm (0, .0001)
  tau.eta <- pow(sigma.eta, -2)
  sigma.eta ~ dunif (0, 100)
}

radon.data <- list ("n", "y", "n.county", "county")

radon.inits <- function(){
  list (mu=rnorm(1), mu.eta=rnorm(1), eta=rnorm(n.county), sigma.y=runif(1),
        sigma.eta=runif(1))
}

radon.parameters <- c ("mu.adj", "eta.adj", "sigma.y", "sigma.eta")
fit.2 <- bugs (radon.data, radon.inits, radon.parameters, model2, n.chains=3,
               n.iter=100, debug=TRUE )
plot(fit.2)
