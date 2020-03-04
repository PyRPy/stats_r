# Chapter 4 sample size ---------------------------------------------------
# following example 2.2 caribou counting...
# 4.1 sample size required for population total
d = 2000
sd2 = 919
N = 286

# following equation 4.7
n0 <- N^2 * qnorm(0.95)^2 * sd2 / d^2
ceiling(n0)

# with the finite population correction factor
n = 1 / (1/n0 + 1/N)
ceiling(n)
