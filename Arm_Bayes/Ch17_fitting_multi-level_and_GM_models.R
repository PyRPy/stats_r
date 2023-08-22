library(R2OpenBUGS)
library ("arm")
library(Matrix)
library(haven)
library(MCMCpack)

# Data preparation for the Chapter 17 -------------------------------------

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

# 17.1 Varying-intercept, varying-slope models ----------------------------

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

model1 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b[county[i]]*x[i]
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    b[j] ~ dnorm (b.hat[j], tau.b)
    a.hat[j] <- mu.a
    b.hat[j] <- mu.b
  }
  mu.a ~ dnorm (0, .0001)
  mu.b ~ dnorm (0, .0001)
  tau.a <- pow(sigma.a, -2)
  tau.b <- pow(sigma.b, -2)
  sigma.a ~ dunif (0, 100)
  sigma.b ~ dunif (0, 100)

}

# with 10 iterations, 3 chains
radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model1,
                      n.chains = 3,
                      n.iter=500,
                      debug=TRUE )

plot (radon.bugs.1)    # to get a plot similar to Figure 16.1
print (radon.bugs.1)   # to display the results in the R console

# Modeling the correlation
model2 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b[county[i]]*x[i]
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] <- B[j,1]
    b[j] <- B[j,2]
    B[j,1:2] ~ dmnorm (B.hat[j,], Tau.B[,])
    B.hat[j,1] <- mu.a
    B.hat[j,2] <- mu.b
  }
  mu.a ~ dnorm (0, .0001)
  mu.b ~ dnorm (0, .0001)

  Tau.B[1:2,1:2] <- inverse(Sigma.B[,])
  Sigma.B[1,1] <- pow(sigma.a, 2)
  sigma.a ~ dunif (0, 100)
  Sigma.B[2,2] <- pow(sigma.b, 2)
  sigma.b ~ dunif (0, 100)
  Sigma.B[1,2] <- rho*sigma.a*sigma.b
  Sigma.B[2,1] <- Sigma.B[1,2]
  rho ~ dunif (-1, 1)

}

# with 500 iterations, 3 chains
radon.bugs.2 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model2,
                      n.chains = 3,
                      n.iter=10000,
                      debug=TRUE )

plot (radon.bugs.2)    # to get a plot similar to Figure 16.1
print (radon.bugs.2)   # to display the results in the R console

# Scaled inverse-Wishart model (same as the "wishart1.bug" file that is
# called in the R code
model3 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b[county[i]]*x[i]
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] <- xi.a*B.raw[j,1]
    b[j] <- xi.b*B.raw[j,2]
    B.raw[j,1:2] ~ dmnorm (B.raw.hat[j,], Tau.B.raw[,])
    B.raw.hat[j,1] <- mu.a.raw
    B.raw.hat[j,2] <- mu.b.raw
  }
  mu.a <- xi.a*mu.a.raw
  mu.b <- xi.b*mu.b.raw
  mu.a.raw ~ dnorm (0, .0001)
  mu.b.raw ~ dnorm (0, .0001)

  xi.a ~ dunif (0, 100)
  xi.b ~ dunif (0, 100)

  Tau.B.raw[1:2,1:2] ~ dwish(W[,], df)
  df <- 3
  Sigma.B.raw[1:2,1:2] <- inverse(Tau.B.raw[,])
  sigma.a <- xi.a*sqrt(Sigma.B.raw[1,1])
  sigma.b <- xi.b*sqrt(Sigma.B.raw[2,2])
  rho <- Sigma.B.raw[1,2]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[2,2])
}

W <- diag(2)
radon.data <- list ("n", "J", "x", "y", "county", "W")
radon.inits <- function (){
  list (B.raw=array(rnorm(2*J), c(J,2)),
        mu.a.raw=rnorm(1),
        mu.b.raw=rnorm(1),
        sigma.y=runif(1),
        Tau.B.raw=rwish(3, diag(2)),
        xi.a=runif(1),
        xi.b=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "mu.b",  "sigma.y", "sigma.a",
                       "sigma.b", "rho")
# with 500 iterations, 3 chains
radon.bugs.3 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model3,
                      n.chains = 3,
                      n.iter=100,
                      debug=TRUE )

plot (radon.bugs.3)    # to get a plot similar to Figure 16.1
print (radon.bugs.3)   # to display the results in the R console
# not converge ver well


# 17.2 Varying intercepts and slopes with group-level predictors ----------
radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J),
        b=rnorm(1),
        mu.a=rnorm(1),
        sigma.y=runif(1),
        sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")

model1 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b[county[i]]*x[i]
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    b[j] ~ dnorm (b.hat[j], tau.b)
    a.hat[j] <- g.a.0 + g.a.1*u[j]
    b.hat[j] <- g.b.0 + g.b.1*u[j]
  }
  g.a.0 ~ dnorm(0, 0.0001)
  g.a.1 ~ dnorm(0, 0.0001)
  g.b.0 ~ dnorm(0, 0.0001)
  g.b.1 ~ dnorm(0, 0.0001)
  tau.a <- pow(sigma.a, -2)
  tau.b <- pow(sigma.b, -2)
  sigma.a ~ dunif (0, 100)
  sigma.b ~ dunif (0, 100)

}

# with 10 iterations, 3 chains
radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model1,
                      n.chains = 3,
                      n.iter=500,
                      debug=TRUE )

plot (radon.bugs.1)    # to get a plot similar to Figure 16.1
print (radon.bugs.1)   # to display the results in the R console
# cannot run the model, u is not defined from a group variable yet


