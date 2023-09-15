
# Chapter 22 ANOVA --------------------------------------------------------
library(R2OpenBUGS)
library ("arm")
library(Matrix)
library(haven)
library(MCMCpack)

# data prep
# 13.5 Non-nested models --------------------------------------------------

pilots <- read.table ("ARM_Data/pilots/pilots.dat", header=TRUE)
head(pilots)
attach(pilots)
group.names <- as.vector(unique(group))
scenario.names <- as.vector(unique(scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group){
  for (k in 1:n.scenario){
    ok <- group==group.names[j] & scenario==scenario.names[k]
    successes <- c (successes, sum(recovered[ok]==1,na.rm=T))
    failures <- c (failures, sum(recovered[ok]==0,na.rm=T))
    group.id <- c (group.id, j)
    scenario.id <- c (scenario.id, k)
  }
}

y <- successes/(successes+failures)
y.mat <- matrix (y, n.scenario, n.group)
sort.group <- order(apply(y.mat,2,mean))
sort.scenario <- order(apply(y.mat,1,mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario,sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln", "Chrlt", "Shemya", "Toledo")

# 22.1 Classical analysis of variance -------------------------------------
## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Classical anova of pilots data
summary (aov (y ~ factor (treatment) + factor(airport)))

#                   Df Sum Sq Mean Sq F value   Pr(>F)
# factor(treatment)  4  0.078  0.0196   0.387    0.816
# factor(airport)    7  3.944  0.5634  11.130 1.19e-06 ***
# Residuals         28  1.417  0.0506

## Model fit
# M1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
# display (M1)


# 22.4 Doing ANOVA using multilevel models --------------------------------

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

model.radon.nopred <- function() {
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]]
    e.y[i] <- y[i] - y.hat[i]
  }
  s.y <- sd(e.y[])
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)
  s.a <- sd(a[])
  for (j in 1:J){
    a[j] ~ dnorm (mu.a, tau.a)
  }
  mu.a ~ dnorm (0, .0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}

radon.data <- list ("n", "J", "y", "county")
radon.inits <- function (){
  list (a=rnorm(J), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
radon.parameters <- c ("a", "mu.a", "sigma.y", "sigma.a", "s.y", "s.a")

anova.radon.nopred <- bugs (radon.data,
                            radon.inits,
                            radon.parameters,
                            model.radon.nopred,
                            n.chains=3,
                            n.iter=2000,
                            debug=TRUE )
attach.bugs (anova.radon.nopred)
print(anova.radon.nopred) # 95% CI for s.a (0.2, 0.4)
#            mean   sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
# a[1]        1.1  0.3    0.6    0.9    1.1    1.2    1.6    1  3000
# a[2]        0.9  0.1    0.7    0.8    0.9    1.0    1.1    1  3000
# a[3]        1.2  0.3    0.7    1.1    1.2    1.4    1.7    1  1400
# a[83]       1.4  0.2    1.1    1.3    1.4    1.5    1.8    1  3000
# a[84]       1.5  0.2    1.1    1.4    1.5    1.6    1.8    1   320
# a[85]       1.3  0.3    0.7    1.1    1.3    1.5    1.8    1  3000
# mu.a        1.3  0.0    1.2    1.3    1.3    1.3    1.4    1   730
# sigma.y     0.8  0.0    0.8    0.8    0.8    0.8    0.8    1  3000
# sigma.a     0.3  0.0    0.2    0.3    0.3    0.3    0.4    1  1200
# s.y         0.8  0.0    0.8    0.8    0.8    0.8    0.8    1  2000
# s.a         0.3  0.0    0.2    0.3    0.3    0.3    0.4    1  1400
# deviance 2194.8 12.7 2171.0 2186.0 2194.0 2203.0 2221.0    1  1800
