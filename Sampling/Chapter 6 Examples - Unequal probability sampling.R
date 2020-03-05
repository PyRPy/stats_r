# Chapter 6 Examples - Unequal probability sampling

# Example 1 animal population survey --------------------------------------

# table 6.1
yi <- c(60, 60, 14, 1)
Area <- 100
n = 4
Length <- c(5, 5, 2, 1)
pi <- Length/Area

# PPS estimator
tau_hat_p <- 1/n * sum(yi/pi)
tau_hat_p

var_tau_hat_p <- 1/((n*(n-1))) * sum((yi/pi - tau_hat_p)^2)
var_tau_hat_p

se_tau_hat_p <- sqrt(var_tau_hat_p)
round(se_tau_hat_p, 0)


# Exmaple 2 Horvitz - Thompson estimator ----------------------------------
yi <- c(60, 14, 1)
Area <- 100
n = 4 # be careful about this number, not n = 3
Length <- c(5, 2, 1)
pi <- Length/Area
pii <- 1 - (1 - pi)^n
tau_pi_hat <- sum(yi/pii)
tau_pi_hat

pi12 <- pii[1] + pii[2] - (1 - (1 - pi[1] - pi[2])^n)
pi13 <- pii[1] + pii[3] - (1 - (1 - pi[1] - pi[3])^n)
pi23 <- pii[2] + pii[3] - (1 - (1 - pi[2] - pi[3])^n)

var_hat_pii <- sum((1/pii^2 - 1/pii) * yi^2) +
              2 * (1/(pii[1]*pii[2]) - 1/pi12) * yi[1]*yi[2] +
              2 * (1/(pii[1]*pii[3]) - 1/pi13) * yi[1]*yi[3] +
              2 * (1/(pii[2]*pii[3]) - 1/pi23) * yi[2]*yi[3]
var_hat_pii  

se_tau_hat_pi <- sqrt(var_hat_pii)  


# Example 4 Small population example --------------------------------------
# wheat production from farms (metric tons)
i <- c(1, 2, 3)
n <- 2
pi <- c(0.3, 0.2, 0.5)
yi <- c(11, 6, 25)
pii <- 1 - (1-pi)^n

# Hanson-Hurwitz estimator
tau_hat_pi <- sum(yi/pii)
tau_hat_pi
