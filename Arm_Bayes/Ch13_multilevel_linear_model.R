# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon
library ("arm")
library(Matrix)
library(haven)
# 12.2 Partial pooling with no predictors ---------------------------------

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


# 13.1 Varying intercepts and slopes --------------------------------------
## Varying intercept & slopes w/ no group level predictors
M3 <- lmer (y ~ x + (1 + x | county))
display (M3)

coef (M3)
fixef (M3)
ranef (M3)


# 13.2 Varying slopes without varying intercepts --------------------------
# not data, psuedo model
# with treatment as a predictor - remove intercept
# fit.1 <- lmer (y ~ T + (T - 1 | county))

# with x as an individual-level predictor
# fit.2 <- lmer (y ~ x + T + (T + x:T - 1 | county))


# 13.4 Understanding correlations between group-level intercepts a --------

heights <- read_dta ("heights.dta")
attach(heights)

# define variables
age <- 90 - yearbn                     # survey was conducted in 1990
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex

# (for simplicity) remove cases with missing data
ok <- !is.na (earn+height+sex) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age,
                                       age.category, eth, male)[ok,])
n <- nrow (heights.clean)
attach(heights.clean)
height.jitter.add <- runif (n, -.2, .2)

# rename variables
y <- log(earn)
x <- height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- age.category

## Regression of log (earnings) on height, age, and ethnicity
length(eth[ok]) # selected rows by 'ok'
M1 <- lmer (y ~ x + (1 + x | eth[ok]))
display (M1)

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

## Model fit
M1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
display (M1)

## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work
# where data was cleaned
# source("13.4_Understanding correlations between intercepts & slopes.R")

## Regression centering the predictors
x.centered <- x - mean(x)
x.centered.jitter <- x.jitter - mean(x)
length(age[ok])
M1 <- lmer (y ~ x.centered + (1 + x.centered | eth[ok]) +
                             (1 + x.centered | age[ok]) +
                             (1 + x.centered | eth[ok]:age[ok]))

M2 <- lmer (y ~ x.centered + (1 + x.centered | eth[ok]))
display (M2)

M3 <- lmer (y ~ x.centered + (1 | eth[ok]) +
                             (1 | age[ok]))
# Error in KhatriRao(sm, t(mm)) : (p <- ncol(X)) == ncol(Y) is not TRUE
