
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


# 3.7 Example: analysis of a bioassay experiment --------------------------
# https://avehtari.github.io/BDA_R_demos/demos_ch3/demo3_6.html
# Data
df1 <- data.frame(
  x = c(-0.86, -0.30, -0.05, 0.73),
  n = c(5, 5, 5, 5),
  y = c(0, 1, 3, 5)
)

# plot data
ggplot(df1, aes(x=x, y=y)) +
  geom_point(size=2, color='red') +
  scale_x_continuous(breaks = df1$x, minor_breaks=NULL, limits = c(-1.5, 1.5)) +
  scale_y_continuous(breaks = 0:5, minor_breaks=NULL) +
  labs(title = 'Bioassay', x = 'Dose (log g/ml)', y = 'Number of deaths')

# Compute the posterior density in grid
A = seq(-4, 8, length.out = 50)
B = seq(-10, 40, length.out = 50)

# Make vectors that contain all pairwise combinations of A and B
cA <- rep(A, each = length(B))
cB <- rep(B, length(A))

# Make a helper function to calculate the log likelihood given a
# dataframe with x, y, and n and evaluation points a and b
logl <- function(df, a, b)
  df['y']*(a + b*df['x']) - df['n']*log1p(exp(a + b*df['x']))

# Calculate likelihoods: apply logl function for each observation ie.
# each row of data frame of x, n and y
# --- very nicely done --- #
p <- apply(df1, 1, logl, cA, cB) %>%
  # sum the log likelihoods of observations
  # and exponentiate to get the joint likelihood
  rowSums() %>% exp()

# Sample from the grid (with replacement)
nsamp <- 1000
samp_indices <- sample(length(p), size = nsamp,
                       replace = T, prob = p/sum(p))
samp_A <- cA[samp_indices[1:nsamp]]
samp_B <- cB[samp_indices[1:nsamp]]

# Add random jitter,
samp_A <- samp_A + runif(nsamp, (A[1] - A[2])/2, (A[2] - A[1])/2)
samp_B <- samp_B + runif(nsamp, (B[1] - B[2])/2, (B[2] - B[1])/2)

# Create data frame
samps <- data_frame(ind = 1:nsamp, alpha = samp_A, beta = samp_B) %>%
  mutate(ld50 = - alpha/beta)

# Create a plot of the posterior density
# limits for the plots
xl <- c(-2, 8)
yl <- c(-2, 40)
pos <- ggplot(data = data.frame(cA ,cB, p), aes(cA, cB)) +
  geom_raster(aes(fill = p, alpha = p), interpolate = T) +
  geom_contour(aes(z = p), colour = 'black', size = 0.2) +
  coord_cartesian(xlim = xl, ylim = yl) +
  labs(title = 'Posterior density evaluated in grid', x = 'alpha', y = 'beta') +
  scale_fill_gradient(low = 'yellow', high = 'red', guide = F) +
  scale_alpha(range = c(0, 1), guide = F)

# Plot of the samples
sam <- ggplot(data = samps) +
  geom_point(aes(alpha, beta), color = 'blue') +
  coord_cartesian(xlim = xl, ylim = yl) +
  labs(title = 'Posterior draws', x = 'alpha', y = 'beta')

# Combine the plots
grid.arrange(pos, sam, nrow=2)

# Plot of the histogram of LD50
his <- ggplot(data = samps) +
  geom_histogram(aes(ld50), binwidth = 0.02,
                 fill = 'steelblue', color = 'black') +
  coord_cartesian(xlim = c(-0.5, 0.5)) +
  labs(x = 'LD50 = -alpha/beta')
his
