
# Single-parameter models -------------------------------------------------
library(ggplot2)
theme_set(theme_minimal())
library(gridExtra)
library(tidyr)
library(dplyr)


# 2.1 Estimating a probability from binomial data -------------------------
# seq creates evenly spaced values
df1 <- data.frame(theta = seq(0.375, 0.525, 0.001))
a <- 438
b <- 544
# dbeta computes the posterior density
df1$p <- dbeta(df1$theta, a, b)

# compute also 95% central interval
df2 <- data.frame(theta = seq(qbeta(0.025, a, b), qbeta(0.975, a, b), length.out = 100))
df2$p <- dbeta(df2$theta, a, b)
hist(df2$theta)
quantile(df2$theta, c(0.025, 0.975))
mean(df2$theta)

# 2.2 Posterior as compromise between data and prior information ----------

# Observed data: 437 girls and 543 boys
a <- 438
b <- 544

# Evaluate densities at evenly spaced points between 0.375 and 0.525
df1 <- data.frame(theta = seq(0.375, 0.525, 0.001))

# Posterior with Beta(1,1), ie. uniform prior
df1$pu <- dbeta(df1$theta, a+1, b+1)
hist(df1$pu)

# 2.3 Summarizing posterior inference -------------------------------------
# Sample from posterior Beta(438,544)
a <- 438
b <- 544
theta <- rbeta(10000, a, b)
hist(theta) # check the spread or shape

#  odds ratio for all draws
phi <- (1 - theta) / theta

# find typical 95% CI
quantiles <- c(0.025, 0.975)
thetaq <- quantile(theta, quantiles)
phiq <- quantile(phi, quantiles)

# prep for the plots
# reshape the data frame or table
df1 <- data.frame(phi, theta) %>% pivot_longer(everything())
df2 <- data.frame(phi=phiq, theta=thetaq) %>% pivot_longer(everything())

ggplot()+
  geom_histogram(data = df1, aes(value), bins=30) +
  geom_vline(data = df2, aes(xintercept=value), linetype='dotted') +
  facet_wrap(~name, ncol = 1, scales = 'free_x') +
  labs(x='', y = '') +
  scale_y_continuous(breaks = NULL)

# 2.4 Informative prior distributions -------------------------------------
# https://avehtari.github.io/BDA_R_demos/demos_ch2/demo2_4.html



# Evaluating posterior with non-conjugate prior in grid
a <- 437
b <- 543
df1 <- data.frame(theta = seq(0.1, 1, 0.001))
df1$con <- dbeta(df1$theta, a, b)

pp <- rep(1, nrow(df1))
# find the index
pi <- sapply(c(0.388, 0.488, 0.588), function(pi) which(df1$theta == pi))
pm <- 11
pp[pi[1]:pi[2]] <- seq(1, pm, length.out = length(pi[1]:pi[2]))
pp[pi[3]:pi[2]] <- seq(1, pm, length.out = length(pi[3]:pi[2]))

# normalize the prior
df1$nc_p <- pp / sum(pp)

# un-normalized non-conjugate posterior in a grid
po <- dbeta(df1$theta, a, b) * pp

# normalize the posterior
df1$nc_po <- po / sum(po)


# Inverse cdf sampling
df1$cs_po <- cumsum(df1$nc_po)

# Sample from uniform distribution U(0,1)
set.seed(2601)
r <- runif(10000)

# Inverse-cdf sampling
invcdf <- function(r, df) df$theta[sum(df$cs_po < r) + 1]

s <- sapply(r, invcdf, df1)
hist(s)
