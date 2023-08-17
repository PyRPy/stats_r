
# 16.3 Fitting and understanding a varying-intercept multilevel mo --------

## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

# source("12.2_Partial pooling with no predictors.R") # where data was cleaned
# close the Bugs window to proceed
library ("arm")
library(R2OpenBUGS)
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


# Classical complete pooling regression -----------------------------------

lm.pooled <- lm (y ~ x)
display (lm.pooled)

# Classical no pooling regression -----------------------------------------


# with the constant term
lm.unpooled.0 <- lm (y ~ x + factor(county))
display (lm.unpooled.0)

# without the constant term
lm.unpooled <- lm (y ~ x + factor(county) - 1)
display (lm.unpooled)


# Call Bugs from R  -------------------------------------------------------

radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J),
        b=rnorm(1),
        mu.a=rnorm(1),
        sigma.y=runif(1),
        sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")

## multilevel regression with varying intercepts and constant slope
# standard deviation given inverse-variance 1/100^2 = 1/10000 = 0.0001
# so the it ranges from -100 to 100 for sigma

model1 <- function(){
   for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b*x[i]
  }
  b ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (mu.a, tau.a)
  }
  mu.a ~ dnorm (0, .0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)

}

# with 10 iterations, 3 chains
radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model1,
                      n.chains = 3,
                      n.iter=10,
                      debug=TRUE )

plot (radon.bugs.1)    # to get a plot similar to Figure 16.1
print (radon.bugs.1)   # to display the results in the R console

# with 500 iterations
radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model1,
                      n.chains = 3,
                      n.iter=500,
                      debug=TRUE )

plot (radon.bugs.1)    # to get Figure 16.1
print (radon.bugs.1)   # to display the results in the R console

#           mean   sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
# a[1]        1.2  0.3    0.7    1.0    1.2    1.4    1.6    1   750
# a[2]        0.9  0.1    0.7    0.9    0.9    1.0    1.1    1   750
# a[3]        1.5  0.3    1.0    1.3    1.5    1.7    2.0    1   750
# a[83]       1.6  0.2    1.2    1.4    1.6    1.7    1.9    1   750
# a[84]       1.6  0.2    1.3    1.5    1.6    1.7    1.9    1   750
# a[85]       1.4  0.3    0.8    1.2    1.4    1.5    1.9    1   460
# b          -0.7  0.1   -0.8   -0.7   -0.7   -0.6   -0.6    1   180
# mu.a        1.5  0.1    1.4    1.4    1.5    1.5    1.6    1   210
# sigma.y     0.8  0.0    0.7    0.7    0.8    0.8    0.8    1   750
# sigma.a     0.3  0.0    0.3    0.3    0.3    0.4    0.4    1   240
# deviance 2094.7 13.7 2071.0 2085.0 2093.0 2103.7 2123.0    1   570

## Summarizing classical and multilevel inferences graphically

# choose counties and construct jittered data
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)  # counties to be displayed
x.jitter <- x + runif(n,-.05,.05)
x.range <- range (x.jitter)
y.range <- range (y[!is.na(match(county,display8))])

# pull out parameter estimates from classical fits
a.pooled <- coef(lm.pooled)[1]           # complete-pooling intercept
b.pooled <- coef(lm.pooled)[2]           # complete-pooling slope
a.nopooled <- coef(lm.unpooled)[2:(J+1)] # no-pooling vector of intercepts
b.nopooled <- coef(lm.unpooled)[1]       # no-pooling slope

# attach Bugs and compute medians
attach.bugs (radon.bugs.1)
a.multilevel <- rep (NA, J)
for (j in 1:J){
  a.multilevel[j] <- median (a[,j])}
b.multilevel <- median (b)

# make the plot in Figure 12.4
par (mfrow=c(2,4))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="floor", ylab="log radon level", main=uniq[j], cex.lab=1.2,
        cex.axis=1.1, cex.main=1.1, mgp=c(2,.7,0), pch=20)
  curve (a.pooled + b.pooled*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (a.nopooled[j] + b.nopooled*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.multilevel[j] + b.multilevel*x, lwd=1, col="black", add=TRUE)
}

# displaying estimates and uncertainties and plot in Figure 12.3b
sample.size <- as.vector (table (county))
sample.size.jitter <- sample.size*exp(runif(J, -.1, .1))

plot (sample.size.jitter, a.multilevel, xlab="sample size in county j",
      ylim=range(y), ylab=expression (paste ("intercept, ", alpha[j],
                                             "   (multilevel inference)")), cex.lab=1.2, cex.axis=1.1, cex.main=1.1,
      mgp=c(2,.7,0), pch=20, log="x")
for (j in 1:J){
  lines (rep(sample.size.jitter[j],2), median(a[,j]) + c(-1,1)*sd(a[,j]))}
abline (a.pooled, 0, lwd=.5)


# 16.5 Adding individual- and group-level predictors ----------------------

## Call Bugs from R
# Note: the Bugs code on pages 353-355 from this section are on #
# the file radon.1.bug and  will be called in the code below    #

radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1),
        sigma.y=runif(1))
}
radon.parameters <- c("a", "b", "sigma.y")

## complete pooling
model.radon.1 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a + b*x[i]
  }
  a ~ dnorm (0, .0001)
  b ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)
}

radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model.radon.1,
                      n.chains=3,
                      n.iter=500,
                      debug=TRUE )
## print the results
print (radon.bugs.1)

# Cumulative: n.sims = 750 iterations saved
#            mean  sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
# a           1.3 0.0    1.3    1.3    1.3    1.3    1.4    1   130
# b          -0.6 0.1   -0.8   -0.7   -0.6   -0.6   -0.5    1   270
# sigma.y     0.8 0.0    0.8    0.8    0.8    0.8    0.9    1   400
# deviance 2250.0 2.4 2247.0 2248.0 2249.0 2251.0 2256.0    1   520
## Accessing the simulations

attach.bugs (radon.bugs.1)

# 90% CI for beta
quantile (b, c(0.05, 0.95))

## No pooling model
radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1),
        sigma.y=runif(1))
}
radon.parameters <- c("a", "b", "sigma.y")


model.radon.2 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b*x[i]
  }
  b ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (0, .0001)
  }
}

radon.bugs.2 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model.radon.2,
                      n.chains=3,
                      n.iter=500,
                      debug=TRUE )
## print the results
print (radon.bugs.2)
# Cumulative: n.sims = 750 iterations saved
#            mean   sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
# a[1]        0.8  0.4    0.0    0.6    0.8    1.1    1.7    1   750
# a[2]        0.9  0.1    0.7    0.8    0.9    0.9    1.1    1   450
# a[3]        1.5  0.5    0.7    1.2    1.5    1.8    2.4    1   690
#
# a[83]       1.6  0.2    1.2    1.5    1.6    1.8    2.0    1   750
# a[84]       1.6  0.2    1.3    1.5    1.6    1.8    2.0    1   190
# a[85]       1.2  0.5    0.1    0.8    1.2    1.6    2.2    1   750
# b          -0.7  0.1   -0.9   -0.8   -0.7   -0.7   -0.6    1   750
# sigma.y     0.8  0.0    0.7    0.7    0.8    0.8    0.8    1   750
# deviance 2096.6 14.3 2071.0 2086.0 2096.0 2106.0 2128.0    1   750

# need to debug this model - not working
## Classical regression with multiple predictors
k = 3
radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(k),
        sigma.y=runif(1))
}
radon.parameters <- c("a", "b", "sigma.y")

model.radon.3 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a + b[1]*x[i] + b[2]*winter[i] + b[3]*x[i]*winter[i]
  }
  for (k in 1:K){
    b[k] ~ dnorm (0, .0001)
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (0, .0001)
  }
}

radon.bugs.3 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model.radon.3,
                      n.chains=3,
                      n.iter=500,
                      debug=TRUE )
## print the results
print (radon.bugs.2)

# 16.6 Predictions for new observations and new groups --------------------
# run the first section to get the data ready for the following sections
## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("ARM_Data/radon/cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

## Extend the dataset
radon.data <- list ("n", "J", "x", "y", "county", "u")
data.save <- save ("n", "y", "county", "x", "u", file="radon.data")
n <- n + 1
y <- c (y, NA)
county <- c (county, 26)
x <- c (x, 1)
u <- c (u, 1)

## Fit the model
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a")
radon.parameters <- c (radon.parameters, "y.tilde")

# varying-intercept model
model.radon.2a <- function() {
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b*x[i]
  }
  y.tilde <- y[n]
  b ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    a.hat[j] <- g.0 + g.1*u[j]
  }
  g.0 ~ dnorm (0, .0001)
  g.1 ~ dnorm (0, .0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}

radon.2a <- bugs(radon.data,
                 radon.inits,
                 radon.parameters,
                 model.radon.2a,
                 n.chains=3,
                 n.iter=500,
                 debug=TRUE )

attach.bugs (radon.2a)
quantile (exp (y.tilde), c(.25, .75))
quantile (exp (y.tilde), c(.025, .975))

#        2.5%     97.5%
#   0.4787585 9.4225108

## New unit in a new group
u.tilde <- mean (u)

load ("radon.data")
data.save <- save ("n", "y", "county", "x", "J", "u", file="radon.data")
n <- n + 1
y <- c (y, NA)
county <- c (county, J+1)
x <- c (x, 1)
J <- J + 1
u <- c (u, u.tilde)

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")
radon.parameters <- c (radon.parameters, "y.tilde")

radon.2b <- bugs (radon.data,
                  radon.inits,
                  radon.parameters,
                  model.radon.2a,
                  n.chains=3,
                  n.iter=500,
                  debug=TRUE )

attach.bugs (radon.2b)
quantile (exp (y.tilde), c(.25, .75))
#        25%      75%
#   1.389960 3.744361

## Prediction using R
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

radon.2c <- bugs (radon.data,
                 radon.inits,
                 radon.parameters,
                 model.radon.2a,
                 n.chains=3,
                 n.iter=500,
                 debug=TRUE )

attach.bugs (radon.2c)

y.tilde <- rnorm (n.sims, a[,26] + b*1, sigma.y)
quantile (exp (y.tilde), c(.25, .75))
#        25%      75%
#   1.165672 3.362505

a.tilde <- rnorm (n.sims, g.0 + g.1*u.tilde, sigma.a)
y.tilde <- rnorm (n.sims, a.tilde + b*1, sigma.y)
quantile (exp (y.tilde), c(.25, .75))
#        25%      75%
#   1.425716 3.878407


# 16.7 Fake-data simulation -----------------------------------------------

## Set up and call Bugs
radon.data <- list ("n", "J", "x", "y", "county", "u")

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

radon.2d <- bugs (radon.data, radon.inits, radon.parameters, model.radon.2a,
                 n.chains=3, n.iter=500, debug=TRUE )

attach.bugs (radon.2d)

## Specifying the unmodeled parameters
b.true <- median (b)
g.0.true <- median (g.0)
g.1.true <- median (g.1)
sigma.y.true <- median (sigma.y)
sigma.a.true <- median (sigma.a)

## Simulating the varying coefficients
a.true <- rep (NA, J)
for (j in 1:J){
  a.true[j] <- rnorm (1, g.0.true + g.1.true*u[j], sigma.a.true)
}

## Simulating fake data
y.fake <- rep (NA, n)
for (i in 1:n){
  y.fake[i] <- rnorm (1, a.true[county[i]] + b.true*x[i], sigma.y.true)
}
hist(y.fake)

## Inference and comparison to "true" values

# specify the data
radon.data.fake <- list (n=n, J=J, y=y.fake, county=county, x=x, u=u)

# specify the rest of inputs

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

radon.2.fake <- bugs (radon.data.fake,
                      radon.inits,
                      radon.parameters,
                      model.radon.2a,
                      n.chains=3,
                      n.iter=500,
                      debug=TRUE )

print (radon.2.fake)

# Current: 3 chains, each with 500 iterations (first 250 discarded)
# Cumulative: n.sims = 750 iterations saved
#            mean   sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
# a[1]        1.0  0.2    0.6    0.9    1.0    1.1    1.4  1.0   750
# a[2]        0.9  0.1    0.7    0.8    0.9    0.9    1.1  1.0   750
# a[3]        1.5  0.2    1.2    1.4    1.5    1.6    1.9  1.0   750
# a[4]        1.1  0.2    0.8    1.0    1.1    1.2    1.4  1.0   220
# a[85]       1.7  0.2    1.3    1.6    1.7    1.8    2.1  1.0   490
# a[86]       1.6  0.2    1.2    1.5    1.6    1.7    2.0  1.0   750
# b          -0.7  0.1   -0.9   -0.8   -0.7   -0.7   -0.6  1.0   750
# sigma.y     0.7  0.0    0.7    0.7    0.7    0.8    0.8  1.0   240
# sigma.a     0.2  0.0    0.1    0.2    0.2    0.2    0.3  1.0   220
# g.0         1.5  0.0    1.4    1.5    1.5    1.5    1.6  1.0   360
# g.1         0.8  0.1    0.6    0.7    0.8    0.9    1.0  1.1    61
# deviance 2060.7 12.2 2037.0 2052.0 2060.0 2070.0 2083.3  1.0   410

## Checking coverage of 50% intervals
attach (radon.2.fake)

# coverage for alpha1
a.true[1] > quantile (a[1,], .25) & a.true[1] < quantile (a[1,], .75)

# coverage for the 85 alphas
cover.50 <- rep (NA, J)
for (j in 1:J){
  cover.50[j] <- a.true[j] > quantile (a, .25) &
    a.true[j] < quantile (a, .75)
}
mean (cover.50)


# 16.8 The principles of modeling in Bugs ---------------------------------

## Set up and call Bugs
radon.data <- list ("n", "J", "x", "y", "county", "u")

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

# varying-intercept model
model.radon.2a <- function() {
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b*x[i]
  }
  y.tilde <- y[n]
  b ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    a.hat[j] <- g.0 + g.1*u[j]
  }
  g.0 ~ dnorm (0, .0001)
  g.1 ~ dnorm (0, .0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}

radon.3 <- bugs (radon.data, radon.inits, radon.parameters, model.radon.2a,
                 n.chains=3, n.iter=500, debug=TRUE )
print(radon.3)


## Define parameters and include them in the data
sigma.y <- .7
sigma.alpha <- .4
radon.data <- list ("n", "J", "x", "y", "county", "u", "sigma.y", "sigma.alpha")

radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1))
}

radon.parameters <- c ("a", "b", "sigma.y", "sigma.alpha", "g.0", "g.1")

radon.3b <- bugs (radon.data, radon.inits, radon.parameters, model.radon.2a,
                 n.chains=3, n.iter=500, debug=TRUE )
print(radon.3b)
