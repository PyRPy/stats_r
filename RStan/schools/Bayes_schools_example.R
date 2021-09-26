# Fitting a hierarchical model in Stan ------------------------------------
# when creating .stan in rstudio, the rstudio crushed a few times, switched
# to direct reading in the 'stan()' function
# ref : http://www.stat.columbia.edu/~gelman/book/software.pdf

# import data
schools <- read.csv("schools/schools.csv", header=TRUE)
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
# eta[4]   -0.03    0.02 0.88  -1.75 -0.62 -0.05  0.55  1.70  1728    1
# eta[5]   -0.36    0.02 0.86  -2.03 -0.93 -0.35  0.20  1.35  2108    1
# eta[6]   -0.20    0.02 0.85  -1.87 -0.77 -0.21  0.38  1.48  2108    1
# eta[7]    0.35    0.02 0.89  -1.42 -0.26  0.36  0.96  2.04  1583    1
# eta[8]    0.04    0.02 0.93  -1.75 -0.62  0.00  0.67  1.85  1942    1
# theta[1] 11.34    0.22 8.44  -2.35  5.78 10.02 15.80 31.78  1423    1
# theta[2]  7.91    0.14 6.28  -4.54  3.90  7.74 11.78 20.88  2024    1
# theta[3]  5.87    0.20 7.82 -11.76  1.73  6.32 10.68 20.55  1563    1
# theta[4]  7.41    0.14 6.54  -5.90  3.45  7.44 11.46 20.15  2263    1
# theta[5]  4.92    0.15 6.13  -8.74  1.39  5.48  9.02 16.08  1762    1
# theta[6]  6.07    0.13 6.40  -8.19  2.44  6.39 10.10 18.19  2263    1
# theta[7] 10.58    0.16 6.97  -1.11  5.92  9.76 14.43 26.68  1914    1
# theta[8]  8.10    0.20 7.87  -7.73  3.69  7.85 12.14 25.29  1523    1
# lp__     -4.86    0.10 2.66 -10.68 -6.51 -4.60 -2.98 -0.37   674    1

# Accessing the posterior simulations
schools_sim = extract(schools_fit)

# display posterior inference for tau
hist(schools_sim$tau)

# posterior probability that the effect is larger in school A than in C
mean(schools_sim$theta[,1] > schools_sim$theta[,3]) # 0.6825

# Posterior predictive simulations and graphs 
n_sims <- length(schools_sim$lp__)
y_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims)
  y_rep[s,] <- rnorm(J, schools_sim$theta[s,], sigma)

par(mfrow=c(5,4), mar=c(4,4,2,2))
hist(y, xlab="", main="y")
for(s in 1:19)
  hist(y_rep[s,], xlab="", main=paste("y_rep",s))

# test statistic such as the difference between the best and second-best 
test <- function(y){
  y_sort <- rev(sort(y))
  return(y_sort[1] - y_sort[2])
}
t_y <- test(y)
t_rep <- rep(NA, n_sims)
for(s in 1:n_sims)
  t_rep[s] <- test(y_rep[s,])

par(mfrow=c(1,1))
cat("T(y) =", round(t_y,1), " and T(y_rep) has mean",
    round(mean(t_rep),1), "and sd", round(sd(t_rep),1),
    "\nPr (T(y_rep) > T(y)) =", round(mean(t_rep>t_y),2), "\n")
hist0 <- hist(t_rep, xlim=range(t_y,t_rep), xlab="T(y_rep)")
lines(rep(t_y,2), c(0,1e6))
text(t_y, .9*max(hist0$count), "T(y)", adj=0)

# Replicated data in new schools
theta_rep <- array(NA, c(n_sims, J))
y_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims){
  theta_rep[s,] <- rnorm(J, schools_sim$mu[s], schools_sim$tau[s])
  y_rep[s,] <- rnorm(J, theta_rep[s,], sigma)
}