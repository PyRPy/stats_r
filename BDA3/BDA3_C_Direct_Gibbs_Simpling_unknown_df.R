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

# Gibbs sampling for the t model with unknown degrees of freedom ----------
log_post <- function(theta, V, mu, tau, nu, y, sigma){
  sum(dnorm(y, theta, sigma,log=TRUE)) +
    sum(dnorm(theta, mu, sqrt(V), log = TRUE)) +
    sum(0.5*nu*log(nu/2) + nu*log(tau) - lgamma(nu/2) - (nu/2+1)*log(V) -
          0.5*nu*tau^2/V)
}

nu_update <- function(sigma_jump_nu){
  nu_inv_star <- rnorm(1, 1/nu, sigma_jump_nu)
  if (nu_inv_star<=0 | nu_inv_star > 1) {
    p_jump = 0
  }
  else {
    nu_star <- 1/nu_inv_star
    log_post_old <- log_post(theta, V, mu, tau, nu, y, sigma)
    log_post_star <- log_post(theta, V, mu, tau, nu_star, y, sigma)
    r <- exp(log_post_star - log_post_old)
    nu <- ifelse(runif(1) < 1, nu_star, nu)
    p_jump <- min(r, 1)
  }

  return (list(nu=nu, p_jump=p_jump)) # return as list, corrected, Appendix C
}

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
sims <- array(NA, c(iter, chains, J+3)) # store results of theta, mu, tau, nu
dimnames(sims) <- list(NULL, NULL,
                       c(paste("theta[", 1:8, "]", sep=""), "mu", "tau", "nu"))
# set the value for 'sigma jump nu' = 1
sigma_jump_nu = 1
p_jump_nu <- array(NA, c(iter, chains))

# run the iterations
for (m in 1:chains){
  mu <- rnorm(1, mean(y), sd(y)) # different initial values for each chain
  tau <- runif(1, 0, sd(y))      # different initial values for each chain
  V <- runif(1, 0, sd(y))^2
  nu <- 1/runif(1, 0, 1)         # new line for unknown nu model
  for (t in 1:iter) {
    theta <- theta_update()
    V <- V_update()
    mu <- mu_update()
    tau <- tau_update()
    temp <- nu_update(sigma_jump_nu)
    nu <- temp$nu
    p_jump_nu[t, m] <- temp$p_jump
    sims[t,m, ] <- c(theta, mu, tau, nu)
  }
}

# check the acceptance rate
print(mean(p_jump_nu)) #  0.1933087

# check the mixing of the sequence
monitor(sims) # this function is from 'rstan' package

# results from this run
#             Q5  Q50  Q95 Mean   SD  Rhat Bulk_ESS Tail_ESS
# theta[1]  -1.0 12.0 35.6 13.9 11.1  1.00      550     1219
# theta[2]  -3.4  7.6 19.5  7.7  7.1  1.00     1548     5841
# theta[3] -13.1  5.2 18.6  4.3  9.8  1.01     1675     3681
# theta[4]  -5.0  7.2 19.9  7.2  7.6  1.00     1556     5143
# theta[5]  -9.1  3.9 14.3  3.5  7.1  1.01     1226     3428
# theta[6]  -8.8  5.1 16.7  4.7  7.8  1.01     1571     4509
# theta[7]   0.3 11.2 25.7 11.9  7.7  1.00      637     1037
# theta[8]  -7.1  8.0 25.0  8.5  9.9  1.00     2104     4959
# mu        -2.2  7.6 17.2  7.5  6.1  1.01      681     1634
# tau        1.3  6.9 22.1  8.6  7.0  1.02      149      282
# nu         1.1  1.9 20.3  8.1 45.4  1.00     1754     1965
