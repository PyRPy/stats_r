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
      epsilon <- runif(1, 0, 2*epsilon_0)
      L <- ceiling(2*L_0*runif(1))
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
  starts[j,] <- rnorm(d, 0, 15)
  starts[j, 10] <- runif(1, 0, 15)
}

# run 20 steps to make sure the program does not crash
M1 <- hmc_run(starting_values = starts, iter = 20, epsilon_0=0.1, L_0=10,
              M=mass_vector)

