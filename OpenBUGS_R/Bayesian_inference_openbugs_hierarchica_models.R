
# Fitting Bayesian Hierarchical Models ------------------------------------

# Section 9.2 / 9.3

library(R2OpenBUGS)

model_h <- function() {
  for (i in 1:N) {
    # likelihood
    y[i] ~ dbin( pi[i], n[i])
    # 2nd stage prior
    pi[i] ~ dbeta( alpha, beta)
  }

  # 3rd stage prior
  alpha ~ dgamma(1.0, 1.0)
  beta ~ dgamma(1.0, 0.33)

  # point of interest
  mu <- alpha / ( alpha + beta )


}

# data as input
mydata <- list(y = c( 1,0,1,1,1,0,1,0), n = c(5,4,3,5,3,4,4,2), N = 8)


# run three Markov Chains
inits0 <- function() {
  list( alpha = 1, beta = 1, pi = c(.5, .5, .5, .5, .5, .5, .5, .5))
  list(alpha = 100, beta = 10, pi = c(.9, .9, .9, .9, .9, .9, .9, .9))
  list(alpha = 10, beta = 100, pi = c(.1, .1, .1, .1, .1, .1, .1, .1))
}

# run the model from R environment
model_out <- bugs(data=mydata,
                  inits = inits0,
                  parameters.to.save = c("alpha", "beta", "mu"),
                  model.file = model_h,
                  n.chains = 3,
                  n.iter = 10000,
                  debug = TRUE)

round(model_out$summary, 3)

#            mean    sd   2.5%    25%    50%    75%  97.5%  Rhat n.eff
# alpha     1.230 0.707  0.306  0.727  1.086  1.570  3.015 1.001 15000
# beta      5.396 3.084  1.296  3.183  4.791  6.913 12.840 1.001  8700
# mu        0.198 0.078  0.074  0.141  0.187  0.244  0.377 1.001 15000
# deviance 14.405 2.562 10.530 12.560 14.030 15.840 20.450 1.001 15000


# Run a predictive case on y ----------------------------------------------
# to find the player's success probability pinew in a future game
# predict number of hits ynew in three at bats

model_pred <- function() {
  for (i in 1:N) {
    # likelihood
    y[i] ~ dbin( pi[i], n[i])
    # 2nd stage prior
    pi[i] ~ dbeta( alpha, beta)
  }

  # 3rd stage prior
  alpha ~ dgamma(1.0, 1.0)
  beta ~ dgamma(1.0, 0.33)

  # point of interest
  mu <- alpha / ( alpha + beta )

  # draw from posterior pred for pi
  pinew ~ dbeta(alpha, beta)

  # draw from posterior pred for new data
  ynew ~ dbin( pinew, nnew )

}

# data as input
data_new <- list(y = c( 1,0,1,1,1,0,1,0),
                 n = c(5,4,3,5,3,4,4,2),
                 N = 8,
                 nnew=3)

# run three Markov Chains
inits_new <- function() {
  list( alpha = 1, beta = 1, pi = c(.5, .5, .5, .5, .5, .5, .5, .5))
  list(alpha = 100, beta = 10, pi = c(.9, .9, .9, .9, .9, .9, .9, .9))
  list(alpha = 10, beta = 100, pi = c(.1, .1, .1, .1, .1, .1, .1, .1))
}

# run the model from R environment
model_pred <- bugs(data=data_new,
                  inits = inits_new,
                  parameters.to.save = c("pinew", "ynew"),
                  model.file = model_pred,
                  n.chains = 3,
                  n.iter = 10000,
                  debug = TRUE)

round(model_pred$summary, 3)

#            mean    sd   2.5%    25%    50%    75%  97.5%  Rhat n.eff
# pinew     0.199 0.178  0.002  0.062  0.151  0.286  0.662 1.001 15000
# ynew      0.594 0.810  0.000  0.000  0.000  1.000  3.000 1.001 15000
# deviance 14.408 2.549 10.540 12.600 14.040 15.790 20.390 1.001 15000
