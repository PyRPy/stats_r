# Chapter 5 Examples - Proportions and ratios

# Example 1 required the sample size --------------------------------------

p = 0.5 # worst scenario
d = 0.05
n = qnorm(0.975)^2 * p * (1 - p)/ d^2
round(n, 0)
