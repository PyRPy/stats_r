
# An introduction to Stan with R ------------------------------------------
# https://www.r-bloggers.com/2019/01/an-introduction-to-stan-with-r/

# library(devtools)
# Sys.setenv(PATH = paste("C:\\Rtools\\bin", Sys.getenv("PATH"), sep=";"))
# Sys.setenv(PATH = paste("C:\\Rtools\\mingw_64\\bin", Sys.getenv("PATH"), sep=";"))
# note : may need to reinstall Rcpp, usethis, before running 'rstan'

library(rstan)


# Step 1: Structure of the problem ----------------------------------------

# Data:
#   
# Parameter:
# 
# Prior distribution
# 
# Likelihood
# 
# Posterior distribution


# Step 2: Stan Program ----------------------------------------------------

bern.stan =
  "
data {
  int<lower=0> N;               // number of trials
  int<lower=0, upper=1> y[N];   // success on trial n
}
parameters {
  real<lower=0, upper=1> theta; // chance of success
}
model {
  theta ~ uniform(0, 1);        // prior
  y ~ bernoulli(theta);         // likelihood
}
"


# Step 3: The data --------------------------------------------------------

# Generate data
theta = 0.30
N = 20
y = rbinom(N, 1, 0.3)
y # 0 0 0 1 1 0 1 0 1 1 0 1 0 0 1 0 0 0 1 0

sum(y) / N # 0.4


# Step 4: Bayesian Posterior using rstan ----------------------------------

fit = stan(model_code=bern.stan, data=list(y=y, N=N), iter=5000)

print(fit, probs=c(0.1, 0.9))
#         mean se_mean   sd    10%    90% n_eff Rhat
# theta   0.41    0.00 0.10   0.28   0.54  3611    1
# lp__  -15.39    0.01 0.73 -16.26 -14.89  4494    1


# Extracting the posterior draws
theta_draws = extract(fit)$theta

# Calculating posterior mean (estimator)
mean(theta_draws) # 0.4097277

# Calculating posterior intervals
quantile(theta_draws, probs=c(0.10, 0.90))
# 10%       90% 
#   0.2778604 0.5435359 


# plot the point estimate
theta_draws_df = data.frame(list(theta=theta_draws))

plotpostre = ggplot(theta_draws_df, aes(x=theta)) +
  geom_histogram(bins=20, color="gray")

plotpostre
