
# understanding and summarizing models ------------------------------------


# 21.5 R2 and explained variance ------------------------------------------

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

# run the first section to get the data ready for the following sections
## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("ARM_Data/radon/cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)

radon.data <- list ("n", "J", "x", "y", "county", "u") # add 'u'variable
radon.inits <- function (){
  list (a=rnorm(J),
        b=rnorm(1),
        sigma.y=runif(1),
        sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "e.y", "E.B")

model1 <- function(){
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b[county[i]]*x[i]
    e.y[i] <- y[i] - y.hat[i]                     # data-level errors
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)

  for (j in 1:J){
    a[j] <- B[j,1]
    b[j] <- B[j,2]
    B[j,1:2] ~ dmnorm (B.hat[j,], Tau.B[,])
    B.hat[j,1] <- g.a.0 + g.a.1*u[j]
    B.hat[j,2] <- g.b.0 + g.b.1*u[j]
    for (k in 1:2){                              # group-level errors
      E.B[j,k] <- B[j,k] - B.hat[j,k]
    }
  }
  Tau.B[1:2,1:2] <- inverse(Sigma.B[,])
  Sigma.B[1,1] <- pow(sigma.a, 2)
  sigma.a ~ dunif (0, 100)
  Sigma.B[2,2] <- pow(sigma.b, 2)
  sigma.b ~ dunif (0, 100)
  Sigma.B[1,2] <- rho*sigma.a*sigma.b
  Sigma.B[2,1] <- Sigma.B[1,2]
  rho ~ dunif (-1, 1)

  g.a.0 ~ dnorm(0, 0.0001)
  g.a.1 ~ dnorm(0, 0.0001)
  g.b.0 ~ dnorm(0, 0.0001)
  g.b.1 ~ dnorm(0, 0.0001)

}

# with 10 iterations, 3 chains
radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model1,
                      n.chains = 3,
                      n.iter=1000,
                      debug=TRUE )

plot (radon.bugs.1)    # to get a plot similar to Figure 16.1
print (radon.bugs.1)   # to display the results in the R console

## Setting up computations for R-Square
attach.bugs(radon.bugs.1)
rsquared <- 1 - mean (apply (e.y, 1, var)) / var (y) # 0.232, fairly low
e.a <- E.B[,,1]
e.b <- E.B[,,2]
rsquared.a <- 1 - mean (apply (e.a, 1, var)) / mean (apply (a, 1, var))
rsquared.b <- 1 - mean (apply (e.b, 1, var)) / mean (apply (b, 1, var))
