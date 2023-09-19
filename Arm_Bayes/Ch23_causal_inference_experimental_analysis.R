library(R2OpenBUGS)
library ("arm")
library(Matrix)
library(haven)
library(MCMCpack)

electric <- read.table ("ARM_Data/electric.company/electric.dat", header=T)
head(electric)
attach(electric)

## Basic analysis of a completely randomized experiment

post.test <- c (treated.Posttest, control.Posttest)
pre.test <- c (treated.Pretest, control.Pretest)
grade <- rep (Grade, 2)
treatment <- rep (c(1,0), rep(length(treated.Posttest),2))
n <- length (post.test)

for (k in 1:4){
  display (lm (post.test ~ treatment, subset=(grade==k)))
}

## Hierarchical model including pair indicators
pair <- rep (1:nrow(electric), 2)
grade.pair <- grade[1:nrow(electric)]
y <- post.test
n <- length(y)
n.grade <- max(grade)
n.pair <- max(pair)

# fitting all 4 grades at once (without controlling for pretest)
model.ch23.1a <- function() {
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y[grade[i]])
    y.hat[i] <- a[pair[i]] + theta[grade[i]]*treatment[i]
  }
  for (j in 1:n.pair){
    a[j] ~ dnorm (mu.a[grade.pair[j]], tau.a[grade.pair[j]])
  }
  for (k in 1:n.grade){
    theta[k] ~ dnorm (0, .0001)
    tau.y[k] <- pow(sigma.y[k], -2)
    sigma.y[k] ~ dunif (0, 100)
    mu.a[k] ~ dnorm (0, .0001)
    tau.a[k] <- pow(sigma.a[k], -2)
    sigma.a[k] ~ dunif (0, 100)
  }
}


data <- c("n", "y", "n.grade", "grade", "grade.pair",
          "treatment", "n.pair", "pair")
inits <- function(){
  list (mu.a=rnorm(n.grade), theta=rnorm(n.grade), a=rnorm(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade))}
params <- c("theta","a","mu.a","sigma.y","sigma.a")

electric.ch23.1a <- bugs (data, inits, params, model.ch23.1a, n.iter=5000,
                          debug=TRUE )
plot (electric.ch23.1a)

# Electric company example with pair indicators: controlling for pre-test
model.ch23.1b <- function() {
  for (i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    y.hat[i] <- a[pair[i]] + theta*treatment[i] + b.pre.test*pre.test[i]
  }
  for (j in 1:n.pair){
    a[j] ~ dnorm(mu.a, tau.a)
  }
  theta ~ dnorm(0, 0.0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif(0, 100)
  mu.a ~ dnorm(0, 0.0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif(0, 100)
  b.pre.test ~ dnorm(0, 0.0001)
}

data <- c("n", "y", "treatment", "n.pair", "pair", "pre.test") # forgot 'y' :-)
inits <- function(){
  list (mu.a=rnorm(n.grade), theta=rnorm(n.grade), a=rnorm(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade),
        b.pre.test=rnorm(n.pair))}
params <- c("theta","a","mu.a","sigma.y","sigma.a", "b.pre.test")

electric.ch23.1b <- bugs (data, inits, params, model.ch23.1b, n.iter=5000,
                          debug=TRUE )
plot (electric.ch23.1b)

# Electric company example: fitting all grades at once
model.ch23.1c <- function() {
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y[grade[i]])
    y.hat[i] <- a[pair[i]] + theta[grade[i]]*treatment[i] +
      b.pre.test[grade[i]]*pre.test[i]
  }
  for (j in 1:n.pair){
    a[j] ~ dnorm (mu.a[grade.pair[j]], tau.a[grade.pair[j]])
  }
  for (k in 1:n.grade){
    theta[k] ~ dnorm (0, .0001)
    tau.y[k] <- pow(sigma.y[k], -2)
    sigma.y[k] ~ dunif (0, 100)
    mu.a[k] ~ dnorm (0, .0001)
    tau.a[k] <- pow(sigma.a[k], -2)
    sigma.a[k] ~ dunif (0, 100)
    b.pre.test[k] ~ dnorm (0, .0001)
  }
}

data <- c("n", "y", "n.grade", "grade", "grade.pair",
          "treatment", "n.pair", "pair", "pre.test")
inits <- function(){
  list (mu.a=rnorm(n.grade), theta=rnorm(n.grade), a=rnorm(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade),
        b.pre.test=rnorm(n.grade))}
params <- c("theta","a","mu.a","sigma.y","sigma.a", "b.pre.test")

electric.ch23.1c <- bugs (data, inits, params, model.ch23.1c, n.iter=5000,
                          debug=TRUE )
plot (electric.ch23.1c)
