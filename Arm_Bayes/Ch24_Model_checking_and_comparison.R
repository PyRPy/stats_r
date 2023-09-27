
# Model checking and comparison -------------------------------------------
library(R2OpenBUGS)
library ("arm")
library(Matrix)
library(haven)
library(MCMCpack)

## Read data and define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/dogs
# data not reading into table correctly

y1 <- as.matrix (read.table ("ARM_Data/dogs/dogs2.dat"), nrows=30, ncol=25)
y1 <- y1[2:31, 2:26]
y <- ifelse (y1[,]=="S",1,0)

## Calling predictive replications in Bugs
n.dogs <- nrow(y)
n.trials <- ncol(y)
data <- list("y", "n.dogs", "n.trials")
inits <- function (){
  list (b.0=rnorm(1), b.1=rnorm(1), b.2=rnorm(1))
}
parameters <- c ("b.0", "b.1", "b.2")

model.dogs.logit.1 <- function() {
  for (j in 1:n.dogs){
    n.avoid[j,1] <- 0
    n.shock[j,1] <- 0
    for (t in 2:n.trials){
      n.avoid[j,t] <- n.avoid[j,t-1] + 1 - y[j,t-1]
      n.shock[j,t] <- n.shock[j,t-1] + y[j,t-1]
    }
    for (t in 1:n.trials){
      y[j,t] ~ dbin (p[j,t], 1)
      logit(p[j,t]) <- b.0 + b.1*n.avoid[j,t] + b.2*n.shock[j,t]
    }
  }
  b.0 ~ dnorm (0, .0001)
  b.1 ~ dnorm (0, .0001)
  b.2 ~ dnorm (0, .0001)
}

fit.logit.1 <- bugs (data, inits,
                     parameters,
                     model.dogs.logit.1,
                     n.chains=3,
                     n.iter=2000,
                     n.thin=100,
                     debug=TRUE )
plot(fit.logit.1)
print(fit.logit.1)
# this result is not correct; data input wrong
#           mean  sd  2.5%   25%   50%   75% 97.5% Rhat n.eff
# b.0        1.1 0.2   0.8   1.0   1.1   1.3   1.5    1  3000
# b.1       -0.3 0.0  -0.4  -0.3  -0.3  -0.3  -0.3    1  1400
# b.2       -0.1 0.0  -0.2  -0.1  -0.1  -0.1   0.0    1  1800
# deviance 676.0 2.5 673.2 674.2 675.4 677.1 682.3    1  3000

# after correct data input
#           mean  sd  2.5%   25%   50%   75% 97.5% Rhat n.eff
# b.0        1.8 0.2   1.4   1.6   1.8   2.0   2.3    1  3000
# b.1       -0.4 0.0  -0.4  -0.4  -0.4  -0.3  -0.3    1  3000
# b.2       -0.2 0.0  -0.3  -0.2  -0.2  -0.2  -0.1    1  3000
# deviance 569.9 2.3 567.1 568.1 569.3 571.0 575.8    1  1100

# Produce simulated data and run the model again --------------------------

## Predictive replications in R
# n.sims <- 1
# n.trials <- 25
# n.dogs <- 30
# y.rep <- array (NA, c(n.sims, n.dogs, n.trials))
# b.0 <- 1.8
# b.1 <- -0.4
# b.2 <- -0.2
# for (j in 1:n.dogs){
#   n.avoid.rep <- rep (0, n.sims)
#   n.shock.rep <- rep (0, n.sims)
#   for (t in 1:n.trials){
#     p.rep <- invlogit (b.0 + b.1*n.avoid.rep + b.2*n.shock.rep)
#     y.rep[,j,t] <- rbinom (n.sims, 1, p.rep)
#     n.avoid.rep <- n.avoid.rep + 1 - y.rep[,j,t]
#     n.shock.rep <- n.shock.rep + y.rep[,j,t]
#   }
# }
#
# n.dogs <- 30
# n.trials <- ncol(y.rep)
# data <- list("y.rep", "n.dogs", "n.trials")
# inits <- function (){
#   list (b.0=rnorm(1), b.1=rnorm(1), b.2=rnorm(1))
# }
# parameters <- c ("b.0", "b.1", "b.2", "y.rep")
#
# model.dogs.logit.2 <- function() {
#   for (j in 1:n.dogs){
#     n.avoid.rep[j,1] <- 0
#     n.shock.rep[j,1] <- 0
#     for (t in 2:n.trials){
#       n.avoid.rep[j,t] <- n.avoid.rep[j,t-1] + 1 - y.rep[j,t-1]
#       n.shock.rep[j,t] <- n.shock.rep[j,t-1] + y.rep[j,t-1]
#     }
#     for (t in 1:n.trials){
#       y.rep[j,t] ~ dbin (p.rep[j,t], 1)
#       logit(p.rep[j,t]) <- b.0 + b.1*n.avoid.rep[j,t] + b.2*n.shock.rep[j,t]
#     }
#   }
#     b.0 ~ dnorm (0, .0001)
#     b.1 ~ dnorm (0, .0001)
#     b.2 ~ dnorm (0, .0001)
# }
#
# fit.logit.2 <- bugs (data, inits,
#                      parameters,
#                      model.dogs.logit.2,
#                      n.chains=3,
#                      n.iter=2000,
#                      n.thin=100,
#                      debug=TRUE )
# plot(fit.logit.1)
# print(fit.logit.1)


# Fitting and checking a logarithmic regression model ---------------------
## Calling predictive replications in Bugs
# error for node y[1,2] of type GraphBinomial.Node first argument must
# be a proportion error pos 37
n.dogs <- nrow(y)
n.trials <- ncol(y)
data <- list("y", "n.dogs", "n.trials")
inits <- function (){
  list (b.1=rnorm(1), b.2=rnorm(1))
}
parameters <- c ("b.1", "b.2")
model.dogs.logit.3 <- function() {
  for (j in 1:n.dogs){
    n.avoid[j,1] <- 0
    n.shock[j,1] <- 0
    for (t in 2:n.trials){
      n.avoid[j,t] <- n.avoid[j,t-1] + 1 - y[j,t-1]
      n.shock[j,t] <- n.shock[j,t-1] + y[j,t-1]
    }
    for (t in 1:n.trials){
      y[j,t] ~ dbin (p[j,t], 1)
      log(p[j,t]) <- b.1*n.avoid[j,t] + b.2*n.shock[j,t]
    }
  }
  b.1 ~ dunif (-100, 0)
  b.2 ~ dunif (-100, 0)
}
# https://github.com/stan-dev/example-models/blob/master/ARM/Ch.24/24.2_BehavioralLearningExperiment.R
# in STan it is look like this
# p.rep <- invlogit (beta[1] + beta[2]*n.avoid.rep + beta[3]*n.schok.rep)
# y.rep[,j,t] <- rbinom (n.sims, 1, p.rep)
fit.logit.3 <- bugs(data, inits,
                     parameters,
                     model.dogs.logit.3,
                     n.chains=3,
                     n.iter=2000,
                     n.thin=100,
                     debug=TRUE )
plot(fit.logit.3)
print(fit.logit.3)
