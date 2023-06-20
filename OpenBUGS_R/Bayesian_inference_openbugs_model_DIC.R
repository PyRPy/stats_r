
# 11.3 The Deviance Information Criterion ---------------------------------
# examples of dyes

library(R2OpenBUGS)

# model - a simple two-stage normal model
dic_model <- function() {
  for (i in 1:batches) {
    mu[i] ~ dnorm(theta, tau.btw)
    for (j in 1: samples) {
      y[i, j] ~ dnorm(mu[i], tau.with)
    }
  }
  sigma2.with <- 1 / tau.with
  sigma2.btw <- 1 / tau.btw
  tau.with ~ dgamma(0.001, 0.001)
  tau.btw ~ dgamma(0.001, 0.001)
  theta ~ dnorm(0.0, 1.0E-10)
}


# data
data1 <- list(batches = 6,
              samples = 5,
              y = structure(
                .Data = c(1545, 1440, 1440, 1520, 1580,
                          1540, 1555, 1490, 1560, 1495,
                          1595, 1550, 1605, 1510, 1560,
                          1445, 1440, 1595, 1465, 1545,
                          1595, 1630, 1515, 1635, 1625,
                          1520, 1455, 1450, 1480, 1445), .Dim = c(6, 5)))

# initial values
inits1 <- function() {
  list(theta = 1500, tau.with = 1, tau.btw = 1)
  list(theta=3000, tau.with=0.1, tau.btw=0.1)
}

# run BUGS model for sampling
model_output <- bugs(data=data1,
                     inits = inits1,
                     parameters.to.save = c("theta", "sigma2.with", "sigma2.btw"),
                     model.file =dic_model,
                     n.chains = 2,
                     n.iter = 25000,
                     debug = TRUE)

# print out result
print(round(model_output$summary, 2))
print(model_output)

#                mean      sd   2.5%     25%     50%     75%   97.5% Rhat n.eff
# theta       1527.04   12.07 1501.0 1520.00 1527.00 1534.00 1551.00 1.01   530
# sigma2.with 4242.93 1202.18 2496.0 3401.00 4039.00 4859.00 7190.00 1.00 25000
# sigma2.btw    98.03  446.17    0.0    0.04    1.13   22.83  897.15 1.00   420
# deviance     334.66    2.16  331.8  333.20  334.10  335.50  340.20 1.00  2200


# DIC - prediction capability example -------------------------------------

# model - a simple two-stage normal model
dic_model2 <- function() {
  for (i in 1:batches) {
    # mu[i] ~ dnorm(theta, tau.btw)
    for (j in 1: samples) {
      # changed to theta from mu[i] as batches mean
      y[i, j] ~ dnorm(theta, tau.with)
    }
  }
  sigma2.with <- 1 / tau.with
  # sigma2.btw <- 1 / tau.btw
  tau.with ~ dgamma(0.001, 0.001)
  # tau.btw ~ dgamma(0.001, 0.001)
  theta ~ dnorm(0.0, 1.0E-10)
}

# data, same as above
# initial values
inits2 <- function() {
  list(theta = 1500, tau.with = 0.1) # removed, tau.btw = 1
  list(theta=0, tau.with=10) # tau.btw=0.1 removed
}

# run BUGS model for sampling
model_output2 <- bugs(data=data1,
                     inits = inits2,
                     parameters.to.save = c("theta", "sigma2.with"),
                     model.file =dic_model2,
                     n.chains = 2,
                     n.iter = 25000,
                     debug = TRUE)

# print out result
print(round(model_output2$summary, 2))
print(model_output2)

#                mean      sd   2.5%    25%    50%    75%   97.5% Rhat n.eff
# theta       1527.49   11.86 1504.0 1520.0 1527.0 1535.0 1551.00    1 25000
# sigma2.with 4266.01 1208.20 2504.0 3411.0 4066.0 4887.0 7203.02    1 25000
# deviance     334.80    2.07  332.8  333.3  334.2  335.6  340.40    1 25000
