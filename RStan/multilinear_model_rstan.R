# Data simulation and model building, sampling ----------------------------
# https://www.r-bloggers.com/2020/01/applied-bayesian-statistics-using-stan-and-r/
set.seed(20190417)
N.sim <- 10000L                               ### num. observations
K.sim <- 5L                                   ### num. predictors
x.sim <- cbind(                               ### model matrix
  rep(1, N.sim), 
  matrix(rnorm(N.sim * (K.sim - 1)), N.sim, (K.sim - 1))
)
beta.sim <- rnorm(K.sim, 0, 10)               ### coef. vector
sigma.sim <- abs(rcauchy(1, 0, 5))            ### scale parameter
mu.sim <- x.sim %*% beta.sim                  ### linear prediction
y.sim <- rnorm(N.sim, mu.sim, sigma.sim)      ### simulated outcome


## Setup
library(rstan)
rstan_options(auto_write = TRUE)             ### avoid recompilation of models
options(mc.cores = parallel::detectCores())  ### parallelize across all CPUs
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native') ### improve execution time

## Data (see data block) as list
standat.sim <- list(
  N = N.sim,
  K = K.sim,
  x = x.sim,
  y = y.sim
)

## C++ Compilation
lm.mod <- stan_model(file = "lm.stan")

## run the sampling 
lm.sim <- sampling(lm.mod,                            ### compiled model
                   data = standat.sim,                ### data input
                   algorithm = "NUTS",                ### algorithm
                   control = list(                    ### control arguments
                     adapt_delta = .85
                   ),
                   save_warmup = FALSE,               ### discard warmup samples
                   sample_file = NULL,                ### no sample file
                   diagnostic_file = NULL,            ### no diagnostic file
                   pars = c("beta", "sigma"),         ### select parameters
                   iter = 2000L,                      ### iter per chain
                   warmup = 1000L,                    ### warmup period
                   thin = 2L,                         ### thinning factor
                   chains = 4L,                       ### num. chains
                   cores = 4L,                        ### num. cores
                   seed = 20190417)                   ### seed


# Parameter estimates from the model: -------------------------------------

print(lm.sim, pars = c("beta", "sigma"))

#          mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# beta[1] -1.06       0 0.12 -1.30 -1.14 -1.06 -0.98 -0.82  1759    1
# beta[2]  5.57       0 0.12  5.34  5.49  5.57  5.65  5.81  1772    1
# beta[3] 17.54       0 0.12 17.30 17.46 17.54 17.62 17.76  1813    1
# beta[4] -1.02       0 0.12 -1.25 -1.10 -1.02 -0.94 -0.78  1666    1
# beta[5] -8.91       0 0.12 -9.14 -8.99 -8.91 -8.83 -8.68  1814    1
# sigma   11.83       0 0.08 11.67 11.77 11.83 11.88 12.00  1685    1


# Step 4: Inference -------------------------------------------------------


# Step 5: Convergence Diagnostics -----------------------------------------


# Visual Diagnostics ------------------------------------------------------

## Extract posterior draws from stanfit object
lm.post.draws <- extract(lm.sim, permuted = FALSE)

## Traceplot
library(bayesplot)
mcmc_trace(lm.post.draws, pars = c("beta[1]", "beta[2]", 
                                   "beta[3]",  "beta[4]",
                                   "beta[5]",
                                   "sigma"))
