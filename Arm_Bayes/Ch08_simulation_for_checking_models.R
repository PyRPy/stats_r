
# 8.1 Fake-data simulation ------------------------------------------------

## Fake-data simulation
library ("arm")

a <- 1.4
b <- 2.3
sigma <- 0.9
x <- 1:5
n <- length(x)

# Simulate data, fit the model, and check the coverage of the conf intervals

y <- a + b*x + rnorm (n, 0, sigma)
lm.1 <- lm (y ~ x)
display (lm.1)

b.hat <- coef (lm.1)[2]       # "b" is the 2nd coef in the model
b.se <- se.coef (lm.1)[2]     # "b" is the 2nd coef in the model

cover.68 <- abs (b - b.hat) < b.se     # this will be TRUE or FALSE
cover.95 <- abs (b - b.hat) < 2*b.se   # this will be TRUE or FALSE
cat (paste ("68% coverage: ", cover.68, "\n"))
cat (paste ("95% coverage: ", cover.95, "\n"))

# Put it in a loop

n.fake <- 1000
cover.68 <- rep (NA, n.fake)
cover.95 <- rep (NA, n.fake)
for (s in 1:n.fake){
  y <- a + b*x + rnorm (n, 0, sigma)
  lm.1 <- lm (y ~ x)
  b.hat <- coef (lm.1)[2]
  b.se <- se.coef (lm.1)[2]
  cover.68[s] <- abs (b - b.hat) < b.se
  cover.95[s] <- abs (b - b.hat) < 2*b.se
}
cat (paste ("68% coverage: ", mean(cover.68), "\n"))
cat (paste ("95% coverage: ", mean(cover.95), "\n"))

# Do it again, this time using t intervals

n.fake <- 1000
cover.68 <- rep (NA, n.fake)
cover.95 <- rep (NA, n.fake)
t.68 <-  qt (.84, n-2)
t.95 <-  qt (.975, n-2)
for (s in 1:n.fake){
  y <- a + b*x + rnorm (n, 0, sigma)
  lm.1 <- lm (y ~ x)
  b.hat <- coef (lm.1)[2]
  b.se <- se.coef (lm.1)[2]
  cover.68[s] <- abs (b - b.hat) < t.68*b.se
  cover.95[s] <- abs (b - b.hat) < t.95*b.se
}
cat (paste ("68% coverage: ", mean(cover.68), "\n"))
cat (paste ("95% coverage: ", mean(cover.95), "\n"))
# may not be true all the time


# 8.2 Example: using fake-data simulation to understand residual p --------

grades <- read.table ("ARM_Data/simulation/gradesW4315.dat", header=TRUE)
head(grades)
midterm <- grades[,"Midterm"]
final <- grades[,"Final"]

## Estimate the model

lm.1 <- lm (final ~ midterm)
display (lm.1)

## Construct fitted values

n <- length(final)
X <- cbind (rep(1,n), midterm) # quiet important to understand this
predicted <- X %*% coef (lm.1)
resid <- lm.1$residuals

## Simulate fake data & compute fitted values

a <- 65
b <- 0.7
sigma <- 15
y.fake <- a + b*midterm + rnorm (n, 0, sigma)

lm.fake <- lm (y.fake ~ midterm)
predicted.fake <- X %*% coef (lm.fake)
resid.fake <- y.fake - predicted.fake

par (mfrow=c(2,2))

## Plots figure 8.1

# plot on the left

plot (predicted, resid, xlab="predicted value", ylab="residual",
      main="Residuals vs. predicted values", pch=20)
abline (0,0, col="gray", lwd=.5)

# plot on the right

plot (final, resid, xlab="observed value", ylab="residual",
      main="Residuals vs. observed values", pch=20)
abline (0,0, col="gray", lwd=.5)

## Plots figure 8.2

# plot on the left

plot (predicted.fake, resid.fake, xlab="predicted value", ylab="residual",
      main="Fake data: resids vs. predicted", pch=20)
abline (0,0, col="gray", lwd=.5)

# plot on the right

plot (y.fake, resid.fake, xlab="observed value", ylab="residual",
      main="Fake data: resids vs. observed", pch=20)
abline (0,0, col="gray", lwd=.5)

par (mfrow=c(1,1))


# 8.3 Simulating from the fitted model and comparing to actual data -------

y <- scan ("lightspeed.dat", skip=4)

## Model fit

light <- lm (y ~ 1)
display (light)

## Create the replicated data

n.sims <- 1000
sim.light <- sim (light, n.sims)

## Create fake data

n <- length (y)
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,] <- rnorm (n, sim.light$coef[s], sim.light$sigma[s])
}

## Histogram of replicated data (Figure 8.4)

par (mfrow=c(5,4), mar=c(3,1,2,1))
for (s in 1:20){
  hist (y.rep[s,], xlab="", ylab="", cex.main="1", yaxt="n", xlim=range(y))
}

## Write a function to make histograms with specified bin widths and ranges

Hist.preset <- function (a, width, ...){
  a.hi <- max (a, na.rm=TRUE)
  a.lo <- min (a, na.rm=TRUE)
  if (is.null(width)) width <- min (sqrt(a.hi-a.lo), 1e-5)
  bin.hi <- width*ceiling(a.hi/width)
  bin.lo <- width*floor(a.lo/width)
  hist (a, breaks=seq(bin.lo,bin.hi,width), ...)
}

## Run the function

par (mfrow=c(5,4), mar=c(3,1,2,1))
for (s in 1:20){
  Hist.preset (y.rep[s,], width=5, xlab="", ylab="", cex.main="1", yaxt="n",
               xlim=range(y), main=paste("Replication #",s,sep=""))
}

## Numerical test

Test <- function (y){
  min (y)
}
test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

## Histogram Figure 8.5

par (mfrow=c(1,1))
hist (test.rep, xlim=range (Test(y), test.rep), yaxt="n", ylab="",
      xlab="", main="Observed T(y) and distribution of T(y.rep)")
lines (rep (Test(y), 2), c(0,10*n))

##############################################################################
## Read the cleaned data
# All data are at http://www.stat.columbia.edu/~gelman/arm/examples/roaches

roachdata <- read.csv ("ARM_Data/roaches/roachdata.csv")
head(roachdata)
attach(roachdata)

## Fit the model

glm.1 <- glm (y ~ roach1 + treatment + senior, family=poisson,
              offset=log(exposure2), data = roachdata)
display (glm.1)

## Comparing the data to a replicated dataset

n <- length(y)
X <- cbind (rep(1,n), roach1, treatment, senior)
y.hat <- exposure2 * exp (X %*% coef(glm.1))
y.rep <- rpois (n, y.hat)

print (mean (y==0))
print (mean (y.rep==0))

## Comparing the data to 1000 replicated datasets

n.sims <- 1000
sim.1 <- sim (glm.1, n.sims)
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.hat <- exposure2 * exp (X %*% coef(sim.1)[s,])
  y.rep[s,] <- rpois (n, y.hat)
}

# test statistic

Test <- function (y){
  mean (y==0)
}
test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

# p-value
print (mean (test.rep > Test(y)))

## Checking the overdispersed model

glm.2 <- glm (y ~ roach1 + treatment + senior, family=quasipoisson,
              offset=log(exposure2), data = roachdata)
display (glm.2)

# 1000 replicated datasets

n.sims <- 1000
sim.2 <- sim (glm.2, n.sims)
y.rep <- array (NA, c(n.sims, n))
overdisp <- summary(glm.2)$dispersion
for (s in 1:n.sims){
  y.hat <- exposure2 * exp (X %*% coef(sim.2)[s,])
  a <- y.hat/(overdisp-1)              # using R's parametrization for the
  y.rep[s,] <- rnegbin (n, y.hat, a)   # negative binomial distribution
}

test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

# p-value
print (mean (test.rep > Test(y)))
# the section above got inconsistant results from what's in the book


# 8.4 Using predictive simulation to check the fit of a time-serie --------

# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/unemployment

unemployment <- read.table ("ARM_Data/unemployment/unemployment.dat",
                            header=TRUE)
year <- unemployment$year
y <- unemployment$unemployed.pct

## Plot of the unemployment rate

par (mar=c(4,4,2,2))
plot (year, y, type="l", ylab="unemployment", xlab="year", yaxs="i",
      ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0), cex.axis=1.2, cex.lab=1.2)
axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0), cex.axis=1.2)

## Fitting a 1st-order autogregression

n <- length (y)
y.lag <- c (NA, y[1:(n-1)])
lm.lag <- lm (y ~ y.lag)
display (lm.lag)

## Simulating replicated datasets

b.hat <- coef (lm.lag)        # vector of 2 regression coefs
s.hat <- sigma.hat (lm.lag)   # residual sd

n.sims <- 1000
y.rep <- array (NA, c(n.sims, n))
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <- c (1, y.rep[s,t-1]) %*% b.hat
    y.rep[s,t] <- rnorm (1, prediction, s.hat)
  }
}

## Including uncertainty in the estimated parameters

lm.lag.sim <- sim (lm.lag, n.sims)       # simulations of beta and sigma
for (s in 1:n.sims){
  y.rep[s,1] <- y[1]
  for (t in 2:n){
    prediction <-  c (1, y.rep[s,t-1]) %*% coef(lm.lag.sim)[s,]
    y.rep[s,t] <- rnorm (1, prediction, sigma.hat(lm.lag.sim)[s])
  }
}

## Plot of simulated unemployment rate series

par (mfrow=c(5,3), mar=c(4,4,2,2))
for (s in 1:15){
  plot (year, y.rep[s,], type="l", ylab="unemployment", xlab="year", yaxs="i",
        ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0),
        main=paste("simulation #", s, sep=""), cex.main=0.95)
  axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0))
}

## Numerical model check

Test <- function (y){
  n <- length (y)
  y.lag <- c (NA, y[1:(n-1)])
  y.lag2 <- c (NA, NA, y[1:(n-2)])
  sum (sign(y-y.lag) != sign(y.lag-y.lag2), na.rm=TRUE)
}

n.sims <- 1000
print (Test (y))
test.rep <- rep (NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,])
}

print (mean (test.rep > Test(y)))
print (quantile (test.rep, c(.05,.5,.95)))
