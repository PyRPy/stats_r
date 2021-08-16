# find CI
# https://online.stat.psu.edu/stat415/lesson/2/2.2
# Example 2-1
n = 126
mu = 29.2
sd = 7.5
alpha = 0.05
Z_inter = qnorm(c(alpha/2, 1-alpha/2))
CI = c(mu + Z_inter * sd / sqrt(n) )

round(CI, 2)
# 27.89 30.51
# 18.2 does not fall into CI