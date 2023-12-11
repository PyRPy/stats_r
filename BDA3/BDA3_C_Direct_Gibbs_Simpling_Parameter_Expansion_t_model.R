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


# Parameter expansion for the t model -------------------------------------
gamma_update <- function(){
  gamma_hat <- (alpha*(y-mu)/sigma^2)/(1/V + alpha^2/sigma^2)
  V_gamma <- 1/(1/V + alpha^2/sigma^2)
  rnorm(J, gamma_hat, sqrt(V_gamma))
}

alpha_update <- function(){
  alpha_hat <- sum(gamma*(y-mu)/sigma^2)/sum(gamma^2/sigma^2)
  V_alpha <- 1/sum(gamma^2/sigma^2)
  rnorm(1, alpha_hat, sqrt(V_alpha))
}

log_post <- function(theta, V, mu, tau, nu, y, sigma){
  sum(dnorm(y, theta, sigma,log=TRUE)) +
    sum(dnorm(theta, mu, sqrt(V), log = TRUE)) +
    sum(0.5*nu*log(nu/2) + nu*log(tau) - lgamma(nu/2) - (nu/2+1)*log(V) -
          0.5*nu*tau^2/V)
}


# theta_update <- function(){
#   theta_hat <- (mu/V + y/sigma^2)/(1/V + 1/sigma^2)
#   V_theta <- 1/(1/V + 1/sigma^2)
#   rnorm(J, theta_hat, sqrt(V_theta))
# }

mu_update <- function(){
  mu_hat <- sum((y-alpha*gamma)/sigma^2)/sum(1/sigma^2)
  V_mu <- 1/sum(1/sigma^2)
  rnorm(1, mu_hat, sqrt(V_mu))
}

tau_update <- function(){
  sqrt(rgamma(1, J*nu/2+1, (nu/2)*sum(1/V)))
}

V_update <- function(){
  (nu*tau^2 + gamma^2)/rchisq(J, nu+1)
}

nu_update <- function(sigma_jump){
  nu_inv_star <- rnorm(1, 1/nu, sigma_jump)
  if (nu_inv_star<=0 | nu_inv_star > 1) {
    p_jump = 0
  }
  else {
    nu_star <- 1/nu_inv_star
    log_post_old <- log_post(mu+alpha*gamma, alpha^2*V, mu, abs(alpha)*tau,
                             nu, y, sigma)
    log_post_star <- log_post(mu+alpha*gamma, alpha^2*V, mu, abs(alpha)*tau,
                              nu_star, y, sigma)
    r <- exp(log_post_star - log_post_old)
    nu <- ifelse(runif(1) < 1, nu_star, nu)
    p_jump <- min(r, 1)
  }

  return (list(nu=nu, p_jump=p_jump)) # return as list, corrected, Appendix C
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
  gamma <- rnorm(J, 0, 1)        # initialization
  alpha <- rnorm(1, 0, 1)        # initialization
  for (t in 1:iter) {
    gamma <- gamma_update()
    alpha <- alpha_update()
    # theta <- theta_update()
    V <- V_update()
    mu <- mu_update()
    tau <- tau_update()
    temp <- nu_update(sigma_jump_nu)
    nu <- temp$nu
    p_jump_nu[t, m] <- temp$p_jump
    sims[t,m, ] <- c(mu+alpha*gamma, mu, abs(alpha)*tau, nu)
  }
}

# check the acceptance rate
print(mean(p_jump_nu)) # 0.1852508

# check the mixing of the sequence
monitor(sims) # this function is from 'rstan' package

# results from this run
#            Q5  Q50  Q95 Mean   SD  Rhat Bulk_ESS Tail_ESS
# theta[1] -0.1 10.1 27.8 11.3  8.6     1     5934     7014
# theta[2] -2.1  7.8 17.8  7.8  6.2     1     9968     9761
# theta[3] -6.7  6.8 17.2  6.2  7.6     1     8599     8536
# theta[4] -2.7  7.6 17.8  7.6  6.4     1     9542     9555
# theta[5] -5.8  5.8 14.6  5.4  6.2     1     7843     9136
# theta[6] -4.9  6.7 16.0  6.3  6.5     1     9470     8757
# theta[7]  0.7  9.8 22.5 10.4  6.7     1     8194     9301
# theta[8] -3.5  8.1 21.4  8.4  7.9     1     8232     8879
# mu       -0.2  7.7 15.9  7.8  5.0     1     4475     5513
# tau       0.3  3.6 14.1  5.0  5.0     1     3745     4108
# nu        1.0  1.9 18.1  7.1 35.3     1     1971     1908
