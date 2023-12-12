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


# --------------------- very good template -------------------------------#
# Programming Hamiltonian Monte Carlo in R --------------------------------

# define a vector for 10 parameters in log posterior density
log_p_th <- function(th, y, sigma){
  J <- length(th) - 2
  theta <- th[1:J]
  mu <- th[J+1]
  tau <- th[J+2]
  if (is.nan(tau) | tau<=0)
    return(-Inf)
  else{
    log_hyperprior <- 1
    log_prior <- sum(dnorm(theta, mu, tau, log = TRUE))
    log_likelihood <- sum(dnorm(y, theta, sigma, log = TRUE))
    return(log_hyperprior + log_prior + log_likelihood)
  }
}

# analytical gradient for derivative of the log posterior wrt each parameter

gradient_th <- function(th, y, sigma){
  J <- length(th) - 2
  theta <- th[1:J]
  mu <- th[J+1]
  tau <- th[J+2]
  if (tau<0)
    return(c(0,0,0))
  else {
    d_theta <- -(theta-y)/sigma^2 - (theta-mu)/tau^2
    d_mu <- -sum(mu-theta)/tau^2
    d_tau <- -J/tau + sum((mu-theta)^2)/tau^3
    return(c(d_theta, d_mu, d_tau))
  }
}

# debugging for differences
gradient_th_numerical <- function(th, y, sigma){
  d <- length(th)
  e <- 0.0001
  diff <- rep(NA, d)
  for (k in 1:d){
    th_hi <- th
    th_lo <- th
    th_hi[k] <- th[k] + e
    th_lo[k] <- th[k] - e
    diff[k] <- (log_p_th(th_hi, y, sigma) - log_p_th(th_lo, y, sigma))/(2*e)
  }
  return(diff)
}

# paramters in HMC
# th - parameter vector
# y  - data
# sigma - data
# epsilon - step size
# L - leapfrog steps per iteration
# M - diagonal mass matrix, as a vector

# return the new value of th and acceptance probability
# the function hmc_iteration performs the L leapfrog steps of an HMC iteration
# and returns the new value of th (same as the old value
# if trajectory was rejected)
hmc_iteration <- function(th, y, sigma, epsilon, L, M) {
  M_inv <- 1/M
  d <- length(th)
  phi <- rnorm(d, 0, sqrt(M))
  th_old <- th
  log_p_old <- log_p_th(th, y, sigma) - 0.5*sum(M_inv*phi^2)
  phi <- phi + 0.5*epsilon*gradient_th(th, y, sigma)
  for (l in 1:L) {
    th <- th + epsilon*M_inv*phi
    phi <- phi + (if(l==L) 0.5 else 1) * epsilon*gradient_th(th, y, sigma)
  }
  phi <- -phi
  log_p_star <- log_p_th(th, y, sigma) - 0.5*sum(M_inv*phi^2)
  r <- exp(log_p_star - log_p_old)
  if (is.nan(r)) r <- 0
  p_jump <- min(r, 1)
  th_new <- if (runif(1) < p_jump) th else th_old
  return(list(th=th_new, p_jump=p_jump))
}

# wrapper function to run HMC iteration
hmc_run <- function(starting_values, iter, epsilon_0, L_0, M) {
  chains <- nrow(starting_values)
  d <- ncol(starting_values)
  sims <- array(NA, c(iter, chains, d),
                dimnames=list(NULL, NULL, colnames(starting_values)))
  warmup <- 0.5*iter
  p_jump <- array(NA, c(iter, chains))
  for (j in 1:chains) {
    th <- starting_values[j,]
    for (t in 1:iter) {
      epsilon <- runif(1, 0, 2*epsilon_0)  # randomly drawn
      L <- ceiling(2*L_0*runif(1))         # randomly drawn
      temp <- hmc_iteration(th, y, sigma, epsilon, L, M)
      p_jump[t, j] <- temp$p_jump
      sims[t, j, ] <- temp$th
      th <- temp$th
    }
  }
  monitor(sims, warmup)
  cat("Avg acceptance prob: ",
      round(colMeans(p_jump[(warmup+1):iter, ]), 2), "\n")
  return(list(sims=sims, p_jump=p_jump))
}

# run the algorithm
# define a vector with names of parameters and select the number of chains
parameter_names <- c(paste("theta[", 1:8, "]", sep=""), "mu", "tau")
d <- length(parameter_names)
chains <- 4

# define a diagonal mass matrix
mass_vector <- rep(1/15^2, d)

# set up an array of random starting points, make sure tau is positive
starts <- array(NA, c(chains, d), dimnames=list(NULL, parameter_names))

for (j in 1:chains) {
  starts[j,] <- rnorm(d, 0, 15)        # normal
  starts[j, 10] <- runif(1, 0, 15)     # uniform
}

# run 20 steps to make sure the program does not crash
M1 <- hmc_run(starting_values = starts, iter = 20, epsilon_0=0.1, L_0=10,
              M=mass_vector)

# results from R console
# Inference for the input samples (4 chains: each with iter = 20; warmup = 10):
#
#             Q5  Q50  Q95 Mean   SD  Rhat Bulk_ESS Tail_ESS
# theta[1]   0.7  5.1 25.1  9.6  8.0  3.05       20       40
# theta[2]  -4.6  7.4 11.9  5.2  5.3  2.25       20       40
# theta[3]  -7.1  0.8 26.3  5.3 10.5  2.69       20       20
# theta[4]  -6.0  7.1 18.5  7.0  6.9  2.05       20       40
# theta[5]  -8.7  3.7  5.8  0.8  5.2  1.71       20       20
# theta[6]  -0.2  6.0  9.7  5.5  3.6  1.13       20       20
# theta[7]   8.5 11.6 17.5 12.3  3.0  1.29       20       20
# theta[8] -14.2  2.6 15.3  1.3  7.9  1.53       20       40
# mu        -0.7  4.4  7.7  3.3  3.8  1.69       20       20
# tau        2.9  8.0 24.2 10.1  6.9  2.48       20       20

# Avg acceptance prob:  0.63 0.48 0.71 0.18

# run for 100 iterations
M2 <- hmc_run(starting_values = starts, iter = 100, epsilon_0=0.1, L_0=10,
              M=mass_vector)
# Avg acceptance prob:  0.5 0.39 0.51 0.68

# decrease step size from 0.1 to 0.05 and set L = 20 to raise acceptance rate
M3 <- hmc_run(starting_values = starts, iter = 100, epsilon_0=0.05, L_0=20,
              M=mass_vector)
# Avg acceptance prob:  0.84 0.28 0.81 0.55

# rerun 10000 iterations for stable inferences
M4 <- hmc_run(starting_values = starts, iter = 10000, epsilon_0=0.05, L_0=20,
              M=mass_vector)
# it takes about 6 seconds to finish
#            Q5  Q50  Q95 Mean  SD  Rhat Bulk_ESS Tail_ESS
# theta[1] -0.5 10.2 27.2 11.3 8.4  1.02      462      768
# theta[2] -2.2  7.7 18.0  7.8 6.2  1.01     1116     4351
# theta[3] -7.5  6.3 17.9  6.0 7.8  1.01     1611     4538
# theta[4] -2.9  7.6 18.8  7.6 6.6  1.01     1235     4266
# theta[5] -6.0  5.2 14.3  5.0 6.2  1.01      880     3234
# theta[6] -5.4  6.1 15.9  5.9 6.5  1.01     1041     4481
# theta[7]  0.5 10.0 23.1 10.6 6.9  1.02      382      843
# theta[8] -3.6  7.9 21.4  8.3 7.8  1.01     1210     4051
# mu       -0.4  7.8 15.9  7.8 5.1  1.01      656      796
# tau       0.4  5.2 16.8  6.5 5.7  1.07       55       29
# Avg acceptance prob:  0.54 0.51 0.6 0.63
