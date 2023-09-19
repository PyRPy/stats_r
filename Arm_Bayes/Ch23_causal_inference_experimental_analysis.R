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


# 23.4 Instrumental variables and multilevel modeling ---------------------

## Read the data & redefine variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sesame

library(foreign)
sesame <- read.dta("ARM_Data/sesame/sesame.dta")
head(sesame)

y=sesame$postlet
d=sesame$regular
yt=cbind(sesame$postlet,sesame$regular)
z=sesame$encour
n=nrow(sesame)

siteset=numeric(nrow(sesame))
for(j in 1:2){
  for(i in 1:5){
    siteset[sesame$site==i & sesame$setting==j]=i+5*(j-1)
  }
}
J=9

## Fit the model for example 1
model.ses <- function() {
  for (i in 1:n){
    yt[i,1:2] ~ dmnorm (yt.hat[i,],Tau.yt[,])      # data model
    yt.hat[i,1] <- a[siteset[i]] + b*yt[i,2]
    yt.hat[i,2] <- g[siteset[i]] + d*z[i]
  }
  for (j in 1:J){
    ag[j,1:2] ~ dmnorm (mu.ag[1:2],Tau.ag[1:2,1:2])
    a[j] <- ag[j,1]
    g[j] <- ag[j,2]
  }

  # data level
  Tau.yt[1:2,1:2] <- inverse(Sigma.yt[,])
  Sigma.yt[1,1] <- pow(sigma.y,2)
  sigma.y ~ dunif (0, 100)                  # noninformative prior on sigma.a
  Sigma.yt[2,2] <- pow(sigma.t,2)
  sigma.t ~ dunif (0, 100)                  # noninformative prior on sigma.b
  Sigma.yt[1,2] <- rho.yt*sigma.y*sigma.t
  Sigma.yt[2,1] <- Sigma.yt[1,2]            # noninformative prior on rho
  rho.yt ~ dunif(-1,1)
  d ~ dnorm (0, .001)
  b ~ dnorm (0, .001)

  # group level
  Tau.ag[1:2,1:2] <- inverse(Sigma.ag[,])
  Sigma.ag[1,1] <- pow(sigma.a,2)
  sigma.a ~ dunif (0, 100)
  Sigma.ag[2,2] <- pow(sigma.g,2)
  sigma.g ~ dunif (0, 100)
  Sigma.ag[1,2] <- rho.ag*sigma.a*sigma.g
  Sigma.ag[2,1] <- Sigma.ag[1,2]
  rho.ag ~ dunif(-1,1)

  mu.ag[1] ~ dnorm (0, .001)
  mu.ag[2] ~ dnorm (0, .001)
}
ses.data <- list("yt","z","n","siteset","J")
ses.params <- c("a","g","d","b","sigma.y","sigma.t","rho.yt","sigma.g",
                "sigma.a","rho.ag")

ses.inits <- function(){
  list(d=rnorm(1,.35,.1),b=rnorm(1),sigma.y=runif(1),sigma.t=runif(1),
       rho.yt=runif(1,-1,1),sigma.a=runif(1),sigma.g=runif(1),
       rho.ag=runif(1,-1,1),ag=cbind(rnorm(J),rnorm(J)))
}

ses <- bugs(ses.data,
            ses.inits,
            ses.params,
            model.ses,
            n.chains=3,
            n.iter=1000,
            debug=TRUE )
print(ses)
# some of the Rhat is higher than 1.1
#            mean  sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
# a[1]       11.0 5.0    1.0    7.5   11.5   14.5   20.0  1.5     7
# a[2]       22.7 4.8   12.8   19.7   22.9   26.0   31.9  1.3    11
# a[3]        8.6 4.4   -0.2    5.4    9.3   11.7   16.4  1.6     7
# g[8]        0.6 0.1    0.5    0.6    0.6    0.7    0.8  1.0  1500
# g[9]        0.2 0.1    0.0    0.1    0.2    0.3    0.4  1.1    27
# d           0.3 0.1    0.2    0.3    0.3    0.4    0.4  1.0    79
# b          14.3 5.0    5.4   10.9   13.5   18.1   24.6  1.5     8
# sigma.y    10.9 0.7    9.5   10.6   11.0   11.2   12.7  1.1    30
# sigma.t     0.4 0.0    0.3    0.3    0.4    0.4    0.4  1.1    26
# rho.yt     -0.1 0.2   -0.4   -0.3   -0.1    0.0    0.2  1.9     5
# sigma.g     0.7 1.0    0.1    0.2    0.2    0.4    3.5  1.9     6
# sigma.a     5.4 1.8    2.8    4.1    5.2    6.4   10.2  1.9     6
# rho.ag      0.0 0.3   -0.5   -0.2    0.0    0.2    0.6  1.1    49
# deviance 2000.8 8.4 1987.5 1995.0 2000.0 2006.0 2020.0  1.0    66
