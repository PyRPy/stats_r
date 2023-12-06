# Fitting a hierarchical model in Stan ------------------------------------
# when creating .stan in rstudio, the rstudio crushed a few times, switched
# to direct reading in the 'stan()' function
# ref : http://www.stat.columbia.edu/~gelman/book/software.pdf

# import data
schools <- read.csv("BDA3_Data/schools.csv", header=TRUE)
J <- nrow(schools)
y <- schools$estimate
sigma <- schools$sd


library(rstan)

# Gibbs sampler for the normal model --------------------------------------

theta_update <- function(){
  theta_hat <- (mu/tau^2 + y/sigma^2)/(1/tau^2 + 1/sigma^2)
  V_theta <- 1/(1/tau^2 + 1/sigma^2)
  rnorm(J, theta_hat, sqrt(V_theta))
}

mu_update <- function(){
  rnorm(1, mean(theta), tau/sqrt(J))
}

tau_update <- function(){
  sqrt(sum((theta-mu)^2)/rchisq(1, J-1))
}

# five independent Gibbs sampling sequences of length 1000
chains <- 5
iter <- 1000
sims <- array(NA, c(iter, chains, J+2)) # store results of theta, mu, tau
dimnames(sims) <- list(NULL, NULL,
                       c(paste("theta[", 1:8, "]", sep=""), "mu", "tau"))

# run the iterations
for (m in 1:chains){
  mu <- rnorm(1, mean(y), sd(y)) # different initial values for each chain
  tau <- runif(1, 0, sd(y))      # different initial values for each chain
  for (t in 1:iter) {
    theta <- theta_update()
    mu <- mu_update()
    tau <- tau_update()
    sims[t,m, ] <- c(theta, mu, tau)
  }
}

# check the mixing of the sequence
monitor(sims) # this function is from 'rstan' package

# results from this run
#            Q5  Q50  Q95 Mean  SD  Rhat Bulk_ESS Tail_ESS
# theta[1]  0.4 10.5 26.5 11.7 8.2  1.02      297      678
# theta[2] -1.8  8.6 19.1  8.5 6.4  1.02      294      235
# theta[3] -6.3  7.4 18.9  6.8 7.7  1.03      202      189
# theta[4] -3.0  8.2 19.4  8.0 6.7  1.03      191      176
# theta[5] -5.9  6.2 15.7  5.7 6.8  1.03      162      152
# theta[6] -4.8  7.1 17.2  6.7 6.7  1.03      211      152
# theta[7]  1.0 10.3 23.0 11.1 6.6  1.03      224      617
# theta[8] -3.2  8.9 21.6  8.9 7.6  1.02      288      319
# mu        0.3  8.4 16.9  8.4 5.2  1.04      140      126
# tau       0.7  5.0 16.1  6.3 5.3  1.06       83      125
