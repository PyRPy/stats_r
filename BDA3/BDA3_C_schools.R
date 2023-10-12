# Fitting a hierarchical model in Stan ------------------------------------
# when creating .stan in rstudio, the rstudio crushed a few times, switched
# to direct reading in the 'stan()' function
# ref : http://www.stat.columbia.edu/~gelman/book/software.pdf

# import data
schools <- read.csv("BDA3_Data/schools.csv", header=TRUE)
J <- nrow(schools)
y <- schools$estimate
sigma <- schools$sd

# stan program
schools.stan = "
data {
    int<lower=0> J;          // number of schools
    real y[J];               // estimated treatment effects
    real<lower=0> sigma[J];  // s.e.’s of effect estimates
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau*eta;
}
model {
  eta ~ normal(0, 1);
  y ~ normal(theta, sigma);
}

"

# fit the model
library(rstan)
schools_fit <- stan(model_code = schools.stan,
                    data=c("J","y","sigma"),
                    iter=1000, chains=4)

print(schools_fit)
#           mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
# mu        7.77    0.16 5.11  -1.72  4.32  7.71 11.17 18.02  1075    1
# tau       6.53    0.19 5.57   0.20  2.42  5.12  9.11 21.00   846    1
# eta[1]    0.39    0.02 0.96  -1.58 -0.24  0.39  1.04  2.18  1819    1
# eta[2]    0.02    0.02 0.92  -1.87 -0.55  0.01  0.62  1.84  1977    1
# eta[3]   -0.20    0.02 0.92  -2.01 -0.81 -0.21  0.39  1.65  2166    1
# theta[6]  6.07    0.13 6.40  -8.19  2.44  6.39 10.10 18.19  2263    1
# theta[7] 10.58    0.16 6.97  -1.11  5.92  9.76 14.43 26.68  1914    1
# theta[8]  8.10    0.20 7.87  -7.73  3.69  7.85 12.14 25.29  1523    1
# lp__     -4.86    0.10 2.66 -10.68 -6.51 -4.60 -2.98 -0.37   674    1

# new run in Oct 2023
#           mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
# mu        7.89    0.17 5.11  -1.89  4.64  7.75 11.07 18.63   889    1
# tau       6.64    0.23 6.03   0.18  2.30  5.13  9.27 20.97   684    1
# eta[1]    0.39    0.02 0.94  -1.49 -0.26  0.43  1.02  2.17  1817    1
# eta[2]   -0.02    0.02 0.85  -1.76 -0.58 -0.02  0.54  1.69  1974    1
# eta[3]   -0.16    0.02 0.92  -1.92 -0.80 -0.15  0.48  1.74  2037    1
# theta[6]  6.11    0.14 6.71  -9.19  2.31  6.46 10.30 18.24  2213    1
# theta[7] 10.60    0.17 6.82  -1.24  5.91 10.11 14.56 25.82  1647    1
# theta[8]  8.26    0.20 7.99  -7.40  3.61  7.92 12.41 26.41  1551    1
# lp__     -4.82    0.11 2.66 -10.49 -6.46 -4.57 -2.99 -0.18   629    1

# Accessing the posterior simulations
schools_sim = extract(schools_fit)

# display posterior inference for tau
hist(schools_sim$tau)

# posterior probability that the effect is larger in school A than in C
mean(schools_sim$theta[,1] > schools_sim$theta[,3]) # 0.6825

# find posterior interval 95%
print(schools_fit, "theta[1]", probs = c(0.025, 0.975))


# Using the t model -------------------------------------------------------
# section 17.4
# https://discourse.mc-stan.org/t/student-t-distribution-a-question-on-scale/16329

schools.stan2 = "
data {
    int<lower=0> J;          // number of schools
    real y[J];               // estimated treatment effects
    real<lower=0> sigma[J];  // s.e.’s of effect estimates
    }
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
  real<lower=1> nu;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau*eta;
}
model {
  // eta ~ normal(0, 1);
  tau ~ cauchy(0, 25);
  eta ~ student_t(nu, 0, 1);
  y ~ normal(theta, sigma);
}

"
schools_fit2 <- stan(model_code = schools.stan2,
                    data=c("J","y","sigma"),
                    iter=1000, chains=4)

print(schools_fit2)
# Rhat >> 1, not converged.
