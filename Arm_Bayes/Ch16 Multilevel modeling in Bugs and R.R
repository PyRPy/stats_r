
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

## Classical complete pooling regression
lm.pooled <- lm (y ~ x)
display (lm.pooled)

## Classical no pooling regression

# with the constant term
lm.unpooled.0 <- lm (y ~ x + factor(county))
display (lm.unpooled.0)

# without the constant term
lm.unpooled <- lm (y ~ x + factor(county) - 1)
display (lm.unpooled)

## Call Bugs from R (remember to save the radon.1.bug file before)
radon.data <- list ("n", "J", "x", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), b=rnorm(1), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "b", "mu.a", "sigma.y", "sigma.a")

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

# with 10 iterations
radon.bugs.1 <- bugs (radon.data,
                      radon.inits,
                      radon.parameters,
                      model.file = model1,
                      n.chains = 1,
                      n.iter=10000,
                      debug=TRUE )

plot (radon.bugs.1)    # to get a plot similar to Figure 16.1
print (radon.bugs.1)   # to display the results in the R console

# with 500 iterations
radon.bugs.1 <- bugs (radon.data, radon.inits, radon.parameters, "radon.1.bug",
                      n.chains=3, debug=TRUE)

plot (radon.bugs.1)    # to get Figure 16.1
print (radon.bugs.1)   # to display the results in the R console

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





