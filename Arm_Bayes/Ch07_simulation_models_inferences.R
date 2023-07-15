
# 7.1 Simulation of probability models ------------------------------------

n.girls <- rbinom (1, 400, .488)
print (n.girls)

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
  n.girls[s] <- rbinom (1, 400, .488)
}
hist (n.girls, main="")

# equivalently

n.sims <- 1000
n.girls <- rbinom (n.sims, 400, .488)
hist (n.girls, main="")

## Accounting for twins

birth.type <- sample (c("fraternal twin", "identical twin", "single birth"),
              size=400, replace=TRUE, prob=c(1/25, 1/300, 1 - 1/25 - 1/300))
girls <- rep (NA, 400)
for (i in 1:400){
  if (birth.type[i]=="single birth"){
    girls[i] <- rbinom (1, 1, .488)}
  else if (birth.type[i]=="identical twin"){
    girls[i] <- 2*rbinom (1, 1, .495)}
  else if (birth.type[i]=="fraternal twin"){
    girls[i] <- rbinom (1, 2, .495)}
}
n.girls <- sum (girls)

# putting in a loop

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
  birth.type <- sample (c("fraternal twin", "identical twin", "single birth"),
                        size=400, replace=TRUE, prob=c(1/25, 1/300, 1 - 1/25 - 1/300))
  girls <- rep (NA, 400)
  for (i in 1:400){
    if (birth.type[i]=="single birth"){
      girls[i] <- rbinom (1, 1, .488)}
    else if (birth.type[i]=="identical twin"){
      girls[i] <- 2*rbinom (1, 1, .495)}
    else if (birth.type[i]=="fraternal twin"){
      girls[i] <- rbinom (1, 2, .495)}
  }
  n.girls[s] <- sum (girls)
}

# or

girls <- ifelse (birth.type=="single birth", rbinom (400, 1, .488),
                 ifelse (birth.type=="identical twin", 2*rbinom (400, 1, .495),
                         rbinom (400, 2, .495)))

## A simple example of continuous predictive simulations

woman <- rbinom (10, 1, .52)
height <- ifelse (woman==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
avg.height <- mean (height)
print(avg.height)

# simulation & Figure 7.1

n.sims <- 1000
avg.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  avg.height[s] <- mean (height)
}
hist (avg.height, main="Average height of 10 adults")

# simulation for the maximum height

n.sims <- 1000
max.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  max.height[s] <- max (height)
}
hist (max.height, main="Maximum height of 10 adults")

## Simulation using custom-made functions

Height.sim <- function (n.adults){
  sex <- rbinom (n.adults, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  return (mean(height))
}

avg.height <- replicate (1000, Height.sim (n.adults=10))
hist (avg.height, main="Average height of 10 adults")

# 7.2 Summarizing linear regressions using simulation: an informal --------

# Bayesian approach -------------------------------------------------------

library ("arm")
library(haven) # read .dta files
heights <- read_dta ("heights.dta")
attach(heights)
# recode sex variable
male <- 2 - sex

# (for simplicity) remove cases with missing data
ok <- !is.na (earn+height+sex) & earn>0
heights.clean <- as.data.frame (cbind (earn, height, male)[ok,])
n <- nrow (heights.clean)

attach(heights.clean)
log.earn <- log(earn)
log10.earn <- log10(earn)
earn.logmodel.3 <- lm(log.earn ~ height + male + height:male, data = heights.clean)
display(earn.logmodel.3)

# Prediction

x.new <- data.frame (height=68, male=1)
pred.interval <- predict (earn.logmodel.3, x.new, interval="prediction",
                          level=.95)

print (exp (pred.interval))

## Constructing the predictive interval using simulation

pred <- exp (rnorm (1000, 9.95, .88))
pred.original.scale <- rnorm (1000, 9.95, .88)

# Histograms (Figure 7.2)

par (mfrow=c(2,2))
hist (pred.original.scale, xlab="log(earnings)", main="")
hist (pred, xlab="earnings", main="")
par (mfrow=c(1,1))
## Why do we need simulation for predictive inferences?

pred.man <- exp (rnorm (1000, 8.4 + 0.17*68 - 0.079*1 + .007*68*1, .88))
pred.woman <- exp (rnorm (1000, 8.4 + 0.17*68 - 0.079*0 + .007*68*0, .88))
pred.diff <- pred.man - pred.woman
pred.ratio <- pred.man/pred.woman

## Simulation to represent uncertainty in regression coefficients

n.sims <- 1000
fit.1<- lm (log.earn ~ height + male + height:male, data = heights.clean)
sim.1 <- sim (fit.1, n.sims)

height.coef <- coef(sim.1)[,2] # use coef() to extract not $ sign
mean (height.coef)
sd (height.coef)
quantile (height.coef, c(.025, .975))

height.for.men.coef <- coef(sim.1)[,2] + coef(sim.1)[,4]
quantile (height.for.men.coef, c(.025, .975))

## Inside the sim function

#for (s in 1: n.sims){
#  sigma[s] <- sigma.hat*sqrt((n-k)/rchisq (1, n-k))
#  beta[s] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
#}
#return (list (coef=beta, sigma=sigma))


# 7.4 Predictive simulation for generalized linear models -----------------

wells <- read.table ("ARM_Data/wells.dat")

## Logistic regression

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"), data = wells)
display (fit.1)

## Predictive simulation using the binomial distribution

n.sims <- 1000
X.tilde <- cbind (1, dist)
n.tilde <- nrow (X.tilde)
y.tilde <- array (NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  p.tilde <- invlogit (X.tilde %*% coef(sim.1)[s, 2]) # this line not working
  y.tilde <- rbinom (n.tilde, 1, p.tilde)
}

## Predictive simulation using latent logistic distribution

logit <- function (a) {log(a/(1-a))}

y.tilde <- array (NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  epsilon.tilde <- logit (runif (n.tilde, 0, 1))
  z.tilde <- X.tilde %*% sim.1$coef[s,] + epsilon.tilde
  y.tilde[s,] <- ifelse (z.tilde>0, 1, 0)
}

# Alternative using matrix algebra

epsilon.tilde <- array (logit (runif (n.sims*n.tilde, 0, 1)),
                        c(n.sims, n.tilde))
z.tilde <- coef(sim.1)[, 2] %*% t(X.tilde) + epsilon.tilde # not working
y.tilde <- ifelse (z.tilde>0, 1, 0)

################################################################################
## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work

### Compound models

## Models
heights.clean$earn.pos <- ifelse (heights.clean$earn>0, 1, 0)
fit.1a <- glm (earn.pos ~ height + male,
               family=binomial(link="logit"),
               data = heights.clean)
display (fit.1a)

heights.clean$log.earn <- log (heights.clean$earn)
fit.1b <- lm (log.earn ~ height + male, subset=earn>0, data = heights.clean)
display (fit.1b)

x.new <- c (1, 68, 1)          # constant term=1, height=68, male=1

# Simulation ignoring uncertainty

n.sims <- 1000
prob.earn.pos <- invlogit (coef(fit.1a) %*% x.new)
earn.pos.sim <- rbinom (n.sims, 1, prob.earn.pos)
earn.sim <- ifelse (earn.pos.sim==0, 0,
                    exp (rnorm (n.sims, coef(fit.1b) %*% x.new, sigma.hat(fit.1b))))

# Simulated values of coefficient estimates

sim.1a <- sim (fit.1a, n.sims)
sim.1b <- sim (fit.1b, n.sims)
prob.earn.pos <- invlogit (coef(sim.1a) %*% x.new)
earn.pos.sim <- rbinom (n.sims, 1, prob.earn.pos)
earn.sim <- ifelse (earn.pos.sim==0, 0,
                    exp (rnorm (n.sims, coef(sim.1b) %*% x.new, sigma.hat(sim.1b))))

# Computations into a function

Mean.earn <- function (height, male, sim.a, sim.b){
  x.new <- c (1, height, male)
  prob.earn.pos <- invlogit (coef(sim.a) %*% x.new)
  earn.pos.sim <- rbinom (n.sims, 1, prob.earn.pos)
  earn.sim <- ifelse (earn.pos.sim==0, 0,
                      exp (rnorm (n.sims, coef(sim.b) %*% x.new, sigma.hat(sim.b))))
  return (mean (earn.sim))
}

heights <- seq (60, 75, 1)
mean.earn.female <- sapply (heights, Mean.earn, male=0, sim.1a, sim.1b)
mean.earn.male <- sapply (heights, Mean.earn, male=1, sim.1a, sim.1b)

# or

heights <- seq (60, 75, 1)
k <- length (heights)
mean.earn.female <- rep (NA, k)
mean.earn.male <- rep (NA, k)
for (i in 1:k){
  mean.earn.female[i] <- Mean.earn (heights[i], 0, sim.1a, sim.1b)
  mean.earn.male[i] <- Mean.earn (heights[i], 1, sim.1a, sim.1b)
}

plot (heights, mean.earn.female, type="l")

