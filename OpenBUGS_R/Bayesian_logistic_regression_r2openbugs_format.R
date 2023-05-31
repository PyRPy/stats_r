
# binary classification in Bayesian method --------------------------------

# https://www.r-bloggers.com/2012/09/the-simplest-species-distribution-model-in-openbugs-r/

# logit function
logit <- function(x) log(x/(1-x))

# "unlogit" function
unlogit <- function(x) exp(x)/(1+exp(x))

# create some data

n.site <- 150  # 150 sites visited
humidity <- sort(runif(n = n.site, min = -1, max =1))
alpha.occ <- 0 # Logit-linear intercept
beta.occ  <- 3 # Logit-linear slope
occ.prob <- unlogit(alpha.occ + beta.occ*humidity)
true.presence <- rbinom(n=n.site, size=1, prob=occ.prob)
plot(true.presence ~ humidity)
lines(occ.prob ~ humidity)


# frequentist analysis

m1 <- glm(true.presence ~ humidity, family="binomial")
summary(m1)
lines(unlogit(predict(m1))~humidity, col="red")

# Bayesian analysis
# transform data or model into format accepted by OPENBUGS
my.data <- list(humidity=humidity,
                true.presence=true.presence,
                n.site=n.site)

library(R2OpenBUGS)

# save the WinBUGS model format into a text file
sink("sdm.txt")
cat("
model
{
  # priors
  alpha.occ ~ dnorm(0,0.001)
  beta.occ ~ dnorm(0,0.001)
  # likelihood
  for(i in 1:n.site)
  {
    logit(p[i]) <- alpha.occ + beta.occ*humidity[i]
    true.presence[i] ~ dbern(p[i])
  }
}
")
sink()

# set parameters
params <- c("alpha.occ", "beta.occ")

# specify initial values
inits <- function()
{
  list (alpha.occ=rnorm(1,0,10),
        beta.occ=rnorm(1,0,10))
}

# run 'BUGS' to sample from the posterior distributions
res <- bugs(data=my.data,
            inits=inits,
            parameters.to.save=params,
            n.iter=2000,
            n.chains=3,
            n.burnin=1000,
            model.file="sdm.txt",
            debug=TRUE,
            codaPkg=TRUE)

# extract data and plot the trends
res.summary <- read.bugs(res)
plot(res.summary)

# quantile and coefficients comparison
quant <- summary(res.summary)$quantiles
alpha.est <- quant[1,3]
beta.est <- quant[2,3]

# predictions based on the Bayesian model
est.prob <- unlogit(alpha.est + beta.est*humidity)

plot(true.presence ~ humidity)
lines(occ.prob ~ humidity)
lines(unlogit(predict(m1))~humidity, col="red")
lines(est.prob ~ humidity, col="green")
