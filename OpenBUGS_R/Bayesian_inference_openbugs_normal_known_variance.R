# Applied Bayesian Statistics 2013
# 8.4.7 OpenBUGS for Normal Models
library(R2OpenBUGS)

# Model 1: known precision, population mean is unknown --------------------

# prediction on 'missing' values

model1 <- function() {
  #likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu, tausq)
  }
  #priors
  mu ~ dnorm(-0.275, 7.5)
}

# data inputs
data1 <- list(y=c(-2.526, -1.715, -1.427, -2.12, -2.659,
                  -2.408, -3.219, -1.966, -2.526, -1.833,
                  -2.813, -1.772, -2.813, -2.526, -3.219,
                  -2.526, -2.813, -2.526, -3.507, -2.996,
                  -3.912, NA),
              N=22,
              tausq=2.5)

# initial values
inits1 <- function() {
  list(mu=-5, mu=-2.5, mu=0)
}

# run the model from R environment
model_mu1 <- bugs(data=data1,
                  inits = inits1,
                  parameters.to.save = c("mu"),
                  model.file = model1,
                  n.chains = 3,
                  n.iter = 10000,
                  debug = TRUE)

# check the results
model_mu1$summary

#               mean        sd  2.5%    25%    50%   75%  97.5%     Rhat n.eff
# mu       -2.276533 0.1286286 -2.53 -2.363 -2.276 -2.19 -2.028 1.000924 15000
# deviance 43.797479 4.0389662 38.73 40.730 42.950 45.94 53.630 1.000948 15000



# Model 2 : both mu and variance are unknown ------------------------------
model2 <- function() {
  #likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu, tausq)
  }
  #priors
  mu ~ dflat()
  tausq ~ dgamma(0.0001, 0.0001)
  sigmasq <- 1/tausq
}

# data inputs
data2 <- list(y=c(-2.526, -1.715, -1.427, -2.12, -2.659,
                  -2.408, -3.219, -1.966, -2.526, -1.833,
                  -2.813, -1.772, -2.813, -2.526, -3.219,
                  -2.526, -2.813, -2.526, -3.507, -2.996,
                  -3.912, NA),
              N=22)

# initial values
# use the format exactly in OPENBUGS
inits2 <- function() {
  list(mu = 0, tausq = 1)
  list(mu = 20, tausq = 100)
  list(mu = 40, tausq = 1000)
}

# run the model from R environment
model_mu2 <- bugs(data=data2,
                  inits = inits2,
                  parameters.to.save = c("mu", "tausq"),
                  model.file = model2,
                  n.chains = 3,
                  n.iter = 10000,
                  debug = TRUE)

# check the results
model_mu2$summary

#               mean        sd      2.5%    25%    50%    75%     97.5%     Rhat n.eff
# mu       -2.563836 0.1420346 -2.846000 -2.657 -2.564 -2.469 -2.283975 1.000907 15000
# tausq     2.590291 0.8239981  1.239975  2.001  2.497  3.080  4.446050 1.001178  7200
# deviance 40.639494 2.0841379 38.600000 39.150 40.000 41.430 46.230250 1.001063 12000



# Model 3: conjugate prior ------------------------------------------------
# normal distribution
# both mu and tausq unknown
# conjugate joint prior

model_mu3 <- function() {
  #likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu, tausq)
  }
  # priors
  tausq0 <- 3 * tausq
  mu ~ dnorm(-0.275, tausq0)
  tausq ~ dgamma(13.3, 5.35)
  sigmasq <- 1 / tausq
}


# initial values
# use the format exactly in OPENBUGS
inits3 <- function() {
  list(mu = 0, tausq = 1)
  list(mu = 20, tausq = 100)
  list(mu = 40, tausq = 1000)
}

model_mu3 <- bugs(data=data2,
                  inits = inits3,
                  parameters.to.save = c("mu", "tausq"),
                  model.file = model_mu3,
                  n.chains = 3,
                  n.iter = 10000,
                  debug = TRUE)

# check the results
model_mu3$summary

#               mean        sd       2.5%    25%    50%    75%  97.5%     Rhat n.eff
# mu       -2.276928 0.1700895 -2.6090250 -2.391 -2.278 -2.162 -1.940 1.001154  7800
# tausq     1.479857 0.3043718  0.9449925  1.266  1.456  1.673  2.132 1.001019 15000
# deviance 45.630361 3.5682835 40.4100000 43.070 45.030 47.570 54.240 1.000916 15000
