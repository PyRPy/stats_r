
# 11.2 Bayes Factors and Bayesian Hypothesis Testing ----------------------
# Ho : mu <= 2.41 Ha: mu > 2.41

library(R2OpenBUGS)
# data from a normal population, precision known, mean or mu unknown
# model
h0_model <- function() {
  for (i in 1:N) {
      # likelihood
      y[i] ~ dnorm(mu, tausq)
    }
  # priors
  mu ~ dnorm(-2.75, 7.5)

  # test H0: mu <= -2.41
  probH0 <- step(-2.41 - mu)

}


# data
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

# run BUGS model for sampling
model_output <- bugs(data=data1,
                     inits = inits1,
                     parameters.to.save = c("mu", "probH0"),
                     model.file = h0_model,
                     n.chains = 3,
                     n.iter = 3000,
                     debug = TRUE)

# print out result
print(round(model_output$summary, 4))
print(model_output)

#             mean     sd   2.5%    25%    50%    75%   97.5%   Rhat n.eff
# mu       -2.5864 0.1270 -2.837 -2.673 -2.585 -2.503 -2.3390 1.0018  1800
# probH0    0.9153 0.2784  0.000  1.000  1.000  1.000  1.0000 1.0010  4500
# deviance 39.4978 1.2258 38.620 38.710 39.030 39.810 43.0157 1.0014  2700
