# Chapter 2 Examples.R ----------------------------------------------------

# Example 2.1 survey fo caribou on a plain
n = 15
N = 286
yi <- c(1, 50, 21, 98, 2, 36, 4, 29, 7, 15, 86, 10, 21, 5, 4)

# sample mean
y_hat <- sum(yi)/length(yi)
y_hat

# sample variance
sd2 <- sum((yi - y_hat)^2)/(n -1)
sd2

sd(yi)^2 # alternatively in base R

# variance of sample mean
# BE CLEAR ABOUT THE DEFINATION AND CONCEPT
var_y_hat <- (N - n)/N * sd2 / n
var_y_hat

# standard error
se_y_hat <- sqrt(var_y_hat)
se_y_hat

# total number
tau_hat <- N * y_hat
tau_hat
# var_tau_hat <- N^2 * sd2 # wrong
var_tau_hat <- N^2 * var_y_hat
var_tau_hat

se_tau_hat <- sqrt(var_tau_hat)
se_tau_hat

# Example 2.2 All possible samples ----------------------------------------
n = 2
N = 4
Unit <- c(1, 2, 3, 4)
yi <- c(10, 17, 13, 20)
tau <- sum(yi)
Ps <- rep(1/choose(4, 2), 6)

# construct data table 2.2
Sample <- c(12, 13, 14, 23, 24, 34)
ys_mean <- apply(rbind(yi[c(1,2)], yi[c(1,3)], yi[c(1,4)], yi[c(2,3)], 
                       yi[c(2,4)], yi[c(3,4)]), 1, mean)
tau_hat <- ys_mean * N
ys_sd <- apply(rbind(yi[c(1,2)], yi[c(1,3)], yi[c(1,4)], yi[c(2,3)], 
                     yi[c(2,4)], yi[c(3,4)]), 1, sd)
sd2 <- ys_sd^2
var_tau_hat <- N * (N - n) * sd2 / n

table2.2 <- data.frame(Sample = Sample,
                       ys_mean = ys_mean,
                       tau_hat = tau_hat,
                       sd2 = sd2, 
                       var_tau_hat = var_tau_hat)
table2.2

# varaince of tau hat
var_tau_hat <- sum((tau_hat - tau)^2 * Ps)
round(var_tau_hat, 3)


# Example 2.3 Random sampling with replacement ----------------------------

yi <- c(2, 4, 0, 4, 5)
n = 5
y_hat_n <- mean(yi)
y_hat_n

y_hat_v <- mean(unique(yi))
y_hat_v
