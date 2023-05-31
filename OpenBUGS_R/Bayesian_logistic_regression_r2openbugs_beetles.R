
# binary classification in Bayesian method --------------------------------
# https://pj.freefaculty.org/Ubuntu/15.04/amd64/openbugs/openbugs-3.2.3/doc/Examples/Beetles.html
# https://www.r-bloggers.com/2012/09/the-simplest-species-distribution-model-in-openbugs-r/

# logit function
logit <- function(x) log(x/(1-x))

# "unlogit" function
unlogit <- function(x) exp(x)/(1+exp(x))

# Bayesian analysis
# transform data or model into format accepted by OPENBUGS
my.data <- list( x = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839),
                 n = c(59, 60, 62, 56, 63, 59, 62, 60),
                 r = c(6, 13, 18, 28, 52, 53, 61, 60), N = 8)

library(R2OpenBUGS)

# save the WinBUGS model format into a text file
sink("logitmodel.txt")
cat("
model
{
  for( i in 1 : N ) {
          r[i] ~ dbin(p[i],n[i])
          logit(p[i]) <- alpha.star + beta * (x[i] - mean(x[]))
          # cloglog(p[i]) <- alpha.star + beta * (x[i] - mean(x[]))
          rhat[i] <- n[i] * p[i]
          # cumulative.r[i] <- cumulative(r[i], r[i])
       }
   alpha <- alpha.star - beta * mean(x[])
   beta ~ dnorm(0.0,0.001)
   alpha.star ~ dnorm(0.0,0.001)
}
")
sink()

# set parameters
params <- c("alpha", "beta", "alpha.star")

# specify initial values
inits <- function()
{
  list(alpha.star=c(0,1), beta=c(0, 1))
}

# run 'BUGS' to sample from the posterior distributions
res <- bugs(data=my.data,
            inits=inits,
            parameters.to.save=params,
            n.iter=10000,
            n.chains=2,
            n.burnin=1000,
            model.file="logitmodel.txt",
            debug=TRUE,
            codaPkg=TRUE)

# extract data and plot the trends
res.summary <- read.bugs(res)
plot(res.summary)

# quantile and coefficients comparison
quant <- summary(res.summary)$quantiles
quant

# print the summary
summary(res.summary)

# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#
#   Mean     SD Naive SE Time-series SE
# alpha      -60.7706 5.1425 0.038330       0.053509
# alpha.star   0.7444 0.1386 0.001033       0.001298
# beta        34.3003 2.8897 0.021539       0.030098
# deviance    39.4302 2.0037 0.014934       0.020345
#
# 2. Quantiles for each variable:
#
#   2.5%      25%      50%      75%   97.5%
# alpha      -71.2800 -64.1625 -60.6200 -57.1875 -51.180
# alpha.star   0.4766   0.6507   0.7423   0.8372   1.017
# beta        28.9097  32.2800  34.2200  36.2000  40.200
# deviance    37.4800  37.9900  38.8000  40.2200  44.880
