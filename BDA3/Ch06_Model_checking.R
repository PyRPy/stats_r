
# Chapter 6 Model checking
library(rstan)
library(ggplot2)
theme_set(theme_minimal())
library(gridExtra)
library(tidyr)
library(dplyr)
library(scales)
library(knitr)
library(MASS)
library(latex2exp)
library(posterior)
library(bayesplot)

# 6.1 The place of model checking in applied Bayesian statistics ----------
# https://avehtari.github.io/BDA_R_demos/demos_ch6/demo6_1.html
# Posterior predictive checking of normal model for light data
# Data
y <- read.table("BDA3_Data/light.txt")$V1

# Sufficient statistics
n <- length(y)
s <- sd(y)
my <- mean(y)

# Create 9 random replicate data sets from the posterior predictive density
yrep <- replicate(9, rt(n, n-1)*sqrt(1+1/n)*s+my)
hist(yrep[, 1])

# Replace one of the replicates with observed data
# If you can spot which one has been replaced,then the model has a flaw
ind <- sample(9, 1)
yrep_df <- yrep %>%
  as.data.frame() %>%
  replace(ind, y) %>% # replace one of the yreps
  setNames(1:9) %>%   # rename to hide which one is the real data
  pivot_longer(everything()) # use the long format for plotting

ggplot(data = yrep_df) +
  geom_histogram(aes(x = value), fill = 'steelblue',
                 color = 'black', binwidth = 4) +
  facet_wrap(~name, nrow = 3) +
  coord_cartesian(xlim = c(-55, 65)) +
  labs(x = '', y = '') +
  scale_y_continuous(breaks=NULL) +
  theme(strip.background = element_blank())
hist(y) # you can spot it somehow, it looks different than the simulated

# Generate 1000 replicate data sets and compute test statistic
# test statistic here is the smallest observation in the data
yrep1000 <- replicate(1000, rt(n, n-1)*sqrt(1+1/n)*s+my) %>%
  as.data.frame()
# the minimum value over 1000 replicates
minvals <- data.frame(x = sapply(yrep1000, min))

# Plot test statistic for the data and the replicated data sets
title1 <- 'Smallest observation in the replicated
data (hist.) vs in the original data (vertical line)'
ggplot(data = minvals) +
  geom_histogram(aes(x = x), fill = 'steelblue',
                 color = 'black', binwidth = 2) +
  geom_vline(aes(xintercept = min(x)), data = data.frame(x = y),
             color = 'red') +
  coord_cartesian(xlim = c(-50, 20)) +
  labs(x = TeX('Minimum of \\textit{y} and \\textit{y}$^{rep}$'),
       y = '', title = title1) +
  scale_y_continuous(breaks=NULL)

#  draws object
rownames(yrep1000) <- paste0("yrep[", 1:66, "]")
yrep_draws <- as_draws_matrix(t(yrep1000))

# Histogram of y + 8 yrep histograms
ppc_hist(y, yrep_draws[1:8,])

# Kernel density estimate of y + 100 yrep kernel density estimates
ppc_dens_overlay(y, yrep_draws[1:100,])

# ECDF of y + 100 yrep ECDFs
ppc_ecdf_overlay(y, yrep_draws[1:100,])

# Scatterplot of yrep vs y
ppc_scatter(y, yrep_draws[1:4,])+geom_abline()

# The distribution of a (test) statistic T(yrep) compared to the observed
# value T(y) computed from the data y.
# The default test statistic mean
ppc_stat(y, yrep_draws)

# Min and max are often good test statistics for continuous outcomes.
ppc_stat(y, yrep_draws, stat="min")
ppc_stat(y, yrep_draws, stat="max")

# Show 2 test statistics in one plot
color_scheme_set("brewer-Paired")
ppc_stat_2d(y, yrep_draws, stat=c("min","max"))

# 6.3 Posterior predictive checking ---------------------------------------
# Checking the assumption of independence in binomial trials
# https://avehtari.github.io/BDA_R_demos/demos_ch6/demo6_2.html
# data
y <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
sum(y == 1) # 7 ones
length(y) - sum(y == 1) # 13 zeros

# Test statistic is the number of switches from 0 to 1 or from 1 to 0.
Ty <- sum(diff(y) != 0) + 0.0

# Sufficient statistics
n <- length(y)
s <- sum(y)

# regenerate test statistic for the replicate data
rb <- function(s, n) {
  p <- rbeta(1, s+1, n-s+1)
  yr <- rbinom(n, 1, p)
  sum(diff(yr) != 0) + 0.0
}
Tyr <- data.frame(x = replicate(10000, rb(s, n)))

# or you can test it manually
p <- rbeta(1, s+1, n-s+1)
yr <- rbinom(n, 1, p)
sum(diff(yr) != 0) + 0.0

# find posterior predictive p-value
mean(Tyr<=Ty) # 0.0285

# Plot test statistics for the data and replicates
# Vertical line corresponds to the original data
# histogram to the replicate data.

title1 <- 'Binomial example - number of changes?
Pr(T(yrep,theta) <= T(y,theta)|y) = 0.03'
ggplot(data = Tyr) +
  geom_histogram(aes(x = x), fill = 'steelblue',
                 color = 'black', binwidth = 1) +
  geom_vline(aes(xintercept = x), data = data.frame(x = Ty),
             color = 'red') +
  labs(x = TeX(r"(Number of changes in $\textit{y}$ and  $\textit{y}^{ \textrm{rep}}$)"),
       y = '', title = title1) +
  scale_y_continuous(breaks=NULL)
