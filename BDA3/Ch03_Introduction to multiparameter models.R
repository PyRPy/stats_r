
# Introduction to multiparameter models -----------------------------------
library(rstan)
library(ggplot2)
theme_set(theme_minimal())
library(gridExtra)
library(tidyr)
library(dplyr)
library(scales)
library(knitr)
# 3.1 Averaging over ‘nuisance parameters’ --------------------------------
# https://avehtari.github.io/BDA_R_demos/demos_ch3/demo3_1_4.html
# Generic part common for Demos 3.1-3.4

# Data
y <- c(93, 112, 122, 135, 122, 150, 118, 90, 124, 114)

# Sufficient statistics
n <- length(y)
s2 <- var(y)
my <- mean(y)

# Factorize the joint posterior p(mu,sigma2|y) to p(sigma2|y)p(mu|sigma2,y)
# Sample from the joint posterior using this factorization

# helper functions to sample from and evaluate
# scaled inverse chi-squared distribution
rsinvchisq <- function(n, nu, s2, ...) nu*s2 / rchisq(n , nu, ...)
dsinvchisq <- function(x, nu, s2){
  exp(log(nu/2)*nu/2 - lgamma(nu/2) + log(s2)/2*nu - log(x)*(nu/2+1) - (nu*s2/2)/x)
}

# Sample 1000 random numbers from p(sigma2|y)
ns <- 1000
sigma2  <- rsinvchisq(ns, n-1, s2)
hist(sigma2) # interesting

# Sample from p(mu|sigma2,y)
mu <- my + sqrt(sigma2/n)*rnorm(length(sigma2))
hist(mu)

# Create a variable sigma and sample from predictive distribution
# p(ynew|y) for each draw of (mu, sigma)
sigma <- sqrt(sigma2)
ynew <- rnorm(ns, mu, sigma)
hist(ynew)


# 3.2 Normal data with a noninformative prior distribution ----------------
# data for light speed measurement
# https://avehtari.github.io/BDA_R_demos/demos_ch3/demo3_5.html

y <- read.table("BDA3_Data/light.txt")$V1

# Sufficient statistics
n <- length(y)
s2 <- var(y)
my <- mean(y)

# Positive values only
y_pos <- y[y > 0]

# Sufficient statistics
n_pos <- length(y_pos)
s2_pos <- var(y_pos)
my_pos <- mean(y_pos)

# Compute the density of mu in these points
tl1 <- c(18, 34)
df1 <- data.frame(t1 = seq(tl1[1], tl1[2], length.out = 1000))

# Compute the exact marginal density for mu
# multiplication by 1./sqrt(s2/n) is due to the transformation of variable
# z=(x-mean(y))/sqrt(s2/n), see BDA3 p. 21
df1$pm_mu <- dt((df1$t1 - my) / sqrt(s2/n), n-1) / sqrt(s2/n) # t-distribution

# Compute the exact marginal density for mu for the positive data
df1$pm_mu_pos = dt((df1$t1 - my_pos) / sqrt(s2_pos/n_pos), n_pos-1) / sqrt(s2_pos/n_pos)

# simple display
hist(df1$pm_mu)
hist(df1$pm_mu_pos)
hist(y[y>0])

