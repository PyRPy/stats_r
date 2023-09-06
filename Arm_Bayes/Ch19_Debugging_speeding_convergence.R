
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


# 19.6 Using redundant parameters to create an informative prior d --------
## Read, clean the pilots data, redefine variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/schools

# Educational testing example
library ("arm")
schools <- read.table ("ARM_data/schools/schools.dat", header=TRUE)
attach.all (schools)
head(schools)
#   school estimate sd
# 1      A       28 15
# 2      B        8 10
# 3      C       -3 16
J <- 8
y <- c(28,8,-3,7,-1,1,18,12)
sigma.y <- c(15,10,16,11,9,11,10,18)
schools.data <- list ("J", "y", "sigma.y")
schools.inits <- function(){
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,100))
}
schools.parameters <- c("theta", "mu.theta", "sigma.theta")

model6 <- function(){
  for (j in 1:J){                               # J = number of schools
    y[j] ~ dnorm (theta[j], tau.y[j])           # data model: the likelihood
    theta[j] <- mu.theta + xi*eta[j]            # see ARM book eq 19.3
    tau.y[j] <- pow(sigma.y[j], -2)
  }
  xi ~ dnorm (0, tau.xi)
  prior.scale <- 25                             # add this into model, works
  tau.xi <- pow(prior.scale, -2)
  for (j in 1:J){
    eta[j] ~ dnorm (0, tau.eta)                 # hierarchical model for theta
  }
  tau.eta ~ dgamma (.5, .5)                     # chi^2 with 1 d.f.
  sigma.theta <- abs(xi)/sqrt(tau.eta)          # cauchy = normal/sqrt(chi^2)
  mu.theta ~ dnorm (0, .0001)                   # noninformative prior on mu
}


# call bugs
schools.sim <- bugs(schools.data, schools.inits, schools.parameters,
                     model6, n.chains=3, n.iter=1000, debug=TRUE )

# display the results on console and graphically
print (schools.sim)
plot (schools.sim)

#             mean  sd 2.5%  25%  50%  75% 97.5% Rhat n.eff
# theta[1]    10.7 7.6 -2.0  5.7  9.9 14.5  29.2    1  1500
# theta[2]     7.9 6.1 -3.7  4.0  7.8 11.6  21.5    1  1500
# theta[3]     6.4 7.2 -8.2  2.7  6.6 10.9  19.6    1  1500
# theta[4]     7.6 6.2 -4.4  3.9  7.6 11.5  20.3    1  1500
# theta[5]     5.3 6.1 -7.8  2.0  5.9  9.4  15.9    1   610
# theta[6]     6.2 6.1 -7.6  2.6  6.5 10.1  17.9    1  1500
# theta[7]    10.0 6.2 -0.8  6.0  9.5 13.3  23.8    1  1500
# theta[8]     8.4 7.5 -5.6  3.9  8.1 12.5  24.3    1  1500
# mu.theta     7.9 4.8 -1.0  4.8  7.8 10.9  17.7    1  1500
# sigma.theta  5.8 4.8  0.2  2.1  4.5  8.1  18.2    1  1500
# deviance    60.2 2.1 56.9 58.9 59.8 61.0  65.2    1  1500

