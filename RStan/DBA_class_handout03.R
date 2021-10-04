# Course Handouts for Bayesian Data Analysis Class ------------------------

# https://bookdown.org/marklhc/notes_bookdown/

library(tidyverse)

# Chapter 3 One-Parameter Models ------------------------------------------

## data set

data("Aids2", package = "MASS")
summary(Aids2$status)
# based on the conjugacy, the posterior of theta is Beta(1,807, 1,116)

# take a large sample of theta from a uniform distribution
th_samples <- runif(1e5, 0, 1)
phi_samples <- log(th_samples / (1 - th_samples)) # transform, logit

ggplot(tibble(phi = phi_samples), aes(x = phi)) + 
  geom_density(bw = "SJ") + 
  labs(x = expression(varphi))

# The beta(1/2, 1/2) is Jeffreys prior

## Poisson Data
# 'A Poisson distribution is suitable for count data in a fixed interval
# For example, think about the number of e-mails for a person each day. 
# If we know on average a person has 20 emails a day, and we assume that 
# emails from different sources arrive independently, then we can model the 
# number of emails in different days as a Poisson distribution.'

# Download data file
# download.file("https://files.osf.io/v1/resources/47tnc/providers/osfstorage/553e51f98c5e4a21991987e7?action=download&version=1&direct", "redcard_data.zip")
redcard_dat <- readr::read_csv("redcard_data.zip") %>% 
  filter(leagueCountry == "England") %>% 
  group_by(player) %>% 
  summarise(rater_dark = (mean(rater1) + mean(rater2)) / 2, 
            yellowCards = sum(yellowCards), 
            redCards = sum(redCards))


# Poisson model and prior are thus
# redCards_i ~ Poisson(lambda)
# log(lambda) ~ Normal(0, sigma_log_lambda)

p1 <- ggplot(tibble(log_lambda = c(-10, 10)), aes(x = log_lambda)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 2.5)) + 
  labs(x = expression(log(lambda)), y = "Density")
p2 <- ggplot(tibble(lambda = c(0, 20)), aes(x = lambda)) + 
  stat_function(fun = function(x) dnorm(log(x), mean = 0, sd = 2.5), 
                n = 501) + 
  labs(x = expression(lambda), y = "Density")
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Getting the posterior
poisson_model = "
data {
  int<lower=0> N;  // number of observations
  int<lower=0> y[N];  // data array (counts);
}
parameters {
  real log_lambda;  // log of rate parameter
}
model {
  y ~ poisson_log(log_lambda);
  // prior
  log_lambda ~ normal(0, 5);
}
generated quantities {
  real lambda = exp(log_lambda);
  int yrep[N];
  for (i in 1:N) {
    yrep[i] = poisson_log_rng(log_lambda);
  }
}
"
library(rstan)

rstan_options(auto_write = TRUE)
m2 <- stan(model_code = poisson_model, 
           data = list(N = 564, y = redcard_dat$redCards), 
           iter = 800, chains = 2, cores = 2)

print(m2, pars = c("log_lambda", "lambda"))

#             mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# log_lambda -0.11       0 0.04 -0.19 -0.14 -0.11 -0.08 -0.02   321    1
# lambda      0.90       0 0.04  0.82  0.87  0.90  0.93  0.98   319    1

# plot thee posterior density
library(bayesplot)
bayesplot::mcmc_areas(m2, pars = c("log_lambda", "lambda"), prob = 0.95)

# Posterior Predictive Check
bayesplot::ppc_bars(redcard_dat$redCards, 
                    yrep = as.matrix(m2, pars = "yrep"))
# not working for this version of the Rcpp 1.0.7 on mac m1

bayesplot::ppc_rootogram(redcard_dat$redCards, 
                         yrep = as.matrix(m2, pars = "yrep"), 
                         style = "hanging")
