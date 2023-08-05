## Read the data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88

# Set up the data for the election88 example

# Load in data for region indicators
# Use "state", an R data file (type ?state from the R command window for info)
#
# Regions:  1=northeast, 2=south, 3=north central, 4=west, 5=d.c.
# We have to insert d.c. (it is the 9th "state" in alphabetical order)

library ("arm")
library(Matrix)
data (state)                  # "state" is an R data file
state.abbr <- c (state.abb[1:8], "DC", state.abb[9:50])
dc <- 9
not.dc <- c(1:8,10:51)
region <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,
            3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

# Load in data from the CBS polls in 1988
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88
library (foreign)
polls <- read.dta ("ARM_Data/election88/polls.dta")
attach(polls)

# Select just the data from the last survey (#9158)
table (survey)                # look at the survey id's
ok <- survey==9158            # define the condition
polls.subset <- polls[ok,]    # select the subset of interest

attach(polls.subset)     # attach the subset
write.table (polls.subset, "polls.subset.dat")

print (polls.subset[1:5,])

# define other data summaries
y <- bush                  # 1 if support bush, 0 if support dukakis
n <- length(y)             # of survey respondents
n.age <- max(age)          # of age categories
n.edu <- max(edu)          # of education categories
n.state <- max(state)      # of states
n.region <- max(region)    # of regions

# compute unweighted and weighted averages for the U.S.
ok <- !is.na(y)                                    # remove the undecideds
cat ("national mean of raw data:", round (mean(y[ok]==1), 3), "\n")
cat ("national weighted mean of raw data:",
     round (sum((weight*y)[ok])/sum(weight[ok]), 3), "\n")

# compute weighted averages for the states
raw.weighted <- rep (NA, n.state)
names (raw.weighted) <- state.abbr
for (i in 1:n.state){
  ok <- !is.na(y) & state==i
  raw.weighted[i] <- sum ((weight*y)[ok])/sum(weight[ok])
}

# load in 1988 election data as a validation check
election88 <- read.dta ("ARM_Data/election88/election88.dta")
outcome <- election88$electionresult

# load in 1988 census data
census <- read.dta ("ARM_Data/election88/census88.dta")

# also include a measure of previous vote as a state-level predictor
presvote <- read.dta ("ARM_Data/election88/presvote.dta")
attach (presvote)
v.prev <- presvote$g76_84pr
not.dc <- c(1:8,10:51)
candidate.effects <- read.table ("ARM_Data/election88/candidate_effects.dat", header=T)
v.prev[not.dc] <- v.prev[not.dc] +
  (candidate.effects$X76 + candidate.effects$X80 + candidate.effects$X84)/3
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88

## Multilevel logistic regression
black <- polls.subset$black
female <- polls.subset$female
state <- polls.subset$state
# changed from lmer to glmer for logistric regression
M1 <- glmer (y ~ black + female + (1 | state), family=binomial(link = "logit"))
display (M1)

## A fuller model

# set up the predictors
age.edu <- n.edu*(age-1) + edu
region.full <- region[state]
v.prev.full <- v.prev[state]

# fit the model
M2 <- glmer (y ~ black + female + black:female + v.prev.full + (1 | age) +
              (1 | edu) + (1 | age.edu) + (1 | state) + (1 | region.full),
             family=binomial(link="logit"))
display (M2)

### Fit the model in Bugs
library(R2OpenBUGS)
model_election88 <- function() {
  for (i in 1:n){
    y[i] ~ dbin (p.bound[i], 1)
    p.bound[i] <- max(0, min(1, p[i]))
    logit(p[i]) <- Xbeta[i]
    Xbeta[i] <- b.0 + b.female*female[i] + b.black*black[i] +
      b.female.black*female[i]*black[i] +
      a.age[age[i]] + a.edu[edu[i]] + a.age.edu[age[i],edu[i]] +
      a.state[state[i]]
  }
  b.0 ~ dnorm (0, .0001)
  b.female ~ dnorm (0, .0001)
  b.black ~ dnorm (0, .0001)
  b.female.black ~ dnorm (0, .0001)

  for (j in 1:n.age) {a.age[j] ~ dnorm(0, tau.age)}
  for (j in 1:n.edu) {a.edu[j] ~ dnorm(0, tau.edu)}
  for (j in 1:n.age) {for (k in 1:n.edu){
    a.age.edu[j,k] ~ dnorm(0, tau.age.edu)}}
  for (j in 1:n.state) {
    a.state[j] ~ dnorm(a.state.hat[j], tau.state)
    a.state.hat[j] <- a.region[region[j]] + b.v.prev*v.prev[j]}
  b.v.prev ~ dnorm (0, .0001)
  for (j in 1:n.region) {a.region[j] ~ dnorm(0, tau.region)}

  tau.age <- pow(sigma.age, -2)
  tau.edu <- pow(sigma.edu, -2)
  tau.age.edu <- pow(sigma.age.edu, -2)
  tau.state <- pow(sigma.state, -2)
  tau.region <- pow(sigma.region, -2)

  sigma.age ~ dunif (0, 100)
  sigma.edu ~ dunif (0, 100)
  sigma.age.edu ~ dunif (0, 100)
  sigma.state ~ dunif (0, 100)
  sigma.region ~ dunif (0, 100)
}

data <- list ("n", "n.age", "n.edu", "n.state", "n.region",
              "y", "female", "black", "age", "edu", "state",
              "region", "v.prev")
inits <- function () {list(
  b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1),
  a.age=rnorm(n.age), a.edu=rnorm(n.edu),
  a.age.edu=array (rnorm(n.age*n.edu), c(n.age,n.edu)),
  a.state=rnorm(n.state), a.region=rnorm(n.region),
  sigma.age=runif(1), sigma.edu=runif(1), sigma.age.edu=runif(1),
  sigma.state=runif(1), sigma.region=runif(1))
}

params <- c ("b.0", "b.female", "b.black", "b.female.black",
             "a.age", "a.edu", "a.age.edu", "a.state", "a.region",
             "sigma.age", "sigma.edu", "sigma.age.edu", "sigma.state",
             "sigma.region")

M2.bugs <- bugs (data = data, inits, params, model.file = model_election88,
                 n.chains=3, n.iter=1000,
                 debug=TRUE )
summary(M2.bugs)
plot(M2.bugs)

## Using the model inferences to estimate avg opinion for each state

# construct the n.sims x 3264 matrix
L <- nrow (census)
n.sims <- 4000
y.pred <- array (NA, c(n.sims, L))
attach.bugs(M2.bugs)
for (l in 1:L){
  y.pred[,l] <- invlogit(b.0 + b.female*census$female[l] +
                           b.black*census$black[l] + b.female.black*census$female[l]*census$black[l] +
                           a.age[,census$age[l]] + a.edu[,census$edu[l]] +
                           a.age.edu[,census$age[l],census$edu[l]] + a.state[,census$state[l]])
}

# average over strata within each state
y.pred.state <- array (NA, c(n.sims, n.state))
for (s in 1:n.sims){
  for (j in 1:n.state){
    ok <- census$state==j
    y.pred.state[s,j] <- sum(census$N[ok]*y.pred[s,ok])/sum(census$N[ok])
  }
}

# average over strata within each state
state.pred <- array (NA, c(n.state,3))
for (j in 1:n.state){
  state.pred[j,] <- quantile (y.pred.state[,j], c(.25,.5,.75))
}
