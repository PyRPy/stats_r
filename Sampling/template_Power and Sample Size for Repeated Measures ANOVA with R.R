# Power and Sample Size for Repeated Measures ANOVA with R ----------------

# https://www.r-bloggers.com/2012/01/power-and-sample-size-for-repeated-measures-anova-with-r/
# Simulation study for sample size between/within
# got Treat + Sham between subjects
# got Time within subjects

# Data preparation --------------------------------------------------------

nPerGroup <- 30
nTime     <- 4
muTreat   <- c(37, 32, 20, 15)
muSham    <- c(37, 32, 25, 22)
stdevs    <- c(12, 10, 8, 6)
stdiff    <- 9
nSim      <- 1000

# set up the indep var data
Subject <- factor(1:(nPerGroup*2))
Time <- factor(1:nTime, labels = c("0min", "15min", "48hrs", "96hrs"))

theData <- expand.grid(Time, Subject)
names(theData) <- c("Time", "Subject")

tmp <- rep(c("Treat", "Sham"), each = nPerGroup * nTime)
theData$Method <- factor(tmp)

head(theData)

# to set up variance-covariance matrix
ones <- rep(1, nTime)
A <- stdevs^2 %o% ones
B <- (A + t(A) + (stdiff^2)*(diag(nTime) - ones %o% ones))/2


# can run it through once to check that it works
library(MASS)
tmp1 <- mvrnorm(nPerGroup, mu = muTreat, Sigma = B)
tmp2 <- mvrnorm(nPerGroup, mu = muSham, Sigma = B)
theData$NDI <- c(as.vector(t(tmp1)), as.vector(t(tmp2)))


# Visualization -----------------------------------------------------------

# some descriptive statistics and graphs
print(model.tables(aovComp, "means"), digits = 3) 
boxplot(NDI ~ Time, data = theData)              
boxplot(NDI ~ Method, data = theData)            
boxplot(NDI ~ Time*Method, data = theData)       
with(theData, interaction.plot(Time, Method, NDI))

# ANOVA MODEL -------------------------------------------------------------

aovComp <- aov(NDI ~ Time*Method + Error(Subject/Time), theData)
summary(aovComp)


###############################################
# for power estimate run the below
# don't forget to set up theData and var-cov
library(MASS)

runTest <- function(){
  tmp1 <- mvrnorm(nPerGroup, mu = muTreat, Sigma = B)
  tmp2 <- mvrnorm(nPerGroup, mu = muSham, Sigma = B)
  theData$NDI <- c(as.vector(t(tmp1)), as.vector(t(tmp2)))
  aovComp <- aov(NDI ~ Time*Method + Error(Subject/Time), theData) 
  b <- summary(aovComp)$'Error: Subject:Time'[[1]][2,5]
  b < 0.05
}

# here is estimate of power for given nPerGroup
mean(replicate(nSim, runTest()))

# to increase "nPerGroup" if power is lower than 80% or 0.8