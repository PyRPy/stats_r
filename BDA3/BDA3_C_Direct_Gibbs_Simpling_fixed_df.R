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

# Gibbs sampling for the t model with fixed degrees of freedom ------------

theta_update <- function(){
  theta_hat <- (mu/V + y/sigma^2)/(1/V + 1/sigma^2)
  V_theta <- 1/(1/V + 1/sigma^2)
  rnorm(J, theta_hat, sqrt(V_theta))
}

mu_update <- function(){
  mu_hat <- sum(theta/V)/sum(1/V)
  V_mu <- 1/sum(1/V)
  rnorm(1, mu_hat, sqrt(V_mu))
}

tau_update <- function(){
  sqrt(rgamma(1, J*nu/2+1, (nu/2)*sum(1/V)))
}

V_update <- function(){
  (nu*tau^2 + (theta-mu)^2)/rchisq(J, nu+1)
}

# five independent Gibbs sampling sequences of length 1000
chains <- 5
iter <- 4000
sims <- array(NA, c(iter, chains, J+2)) # store results of theta, mu, tau
dimnames(sims) <- list(NULL, NULL,
                       c(paste("theta[", 1:8, "]", sep=""), "mu", "tau"))
# fix the df at 4
nu = 4
# run the iterations
for (m in 1:chains){
  mu <- rnorm(1, mean(y), sd(y)) # different initial values for each chain
  tau <- runif(1, 0, sd(y))      # different initial values for each chain
  V <- runif(1, 0, sd(y))^2
  for (t in 1:iter) {
    theta <- theta_update()
    V <- V_update()
    mu <- mu_update()
    tau <- tau_update()
    sims[t,m, ] <- c(theta, mu, tau)
  }
}

# check the mixing of the sequence
monitor(sims) # this function is from 'rstan' package

# need to longer iteration to reduce R below 1.05

# results from this run
#             Q5  Q50  Q95 Mean   SD  Rhat Bulk_ESS Tail_ESS
# theta[1]   0.2 13.8 36.2 15.5 11.1  1.00      556     2862
# theta[2]  -4.4  7.8 20.7  8.0  7.6  1.00     3975     7395
# theta[3] -14.2  5.0 19.9  4.2 10.4  1.00     2738     3623
# theta[4]  -5.8  7.5 20.6  7.5  7.9  1.00     4116     5822
# theta[5] -10.2  3.7 14.5  3.1  7.5  1.00     1282     4046
# theta[6]  -9.7  5.1 17.3  4.6  8.2  1.00     2037     4181
# theta[7]   0.9 12.2 27.3 12.9  8.1  1.00      839     4394
# theta[8]  -7.5  8.7 27.8  9.3 10.7  1.00     3925     4115
# mu        -1.9  8.0 18.9  8.2  6.7  1.00     1783     2413
# tau        2.1  9.3 27.8 11.5  8.7  1.03      107       81
