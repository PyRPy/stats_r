
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
