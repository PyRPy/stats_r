# 6.1 - Estimating a Mean -------------------------------------------------

# https://online.stat.psu.edu/stat415/lesson/5/5.1
# Example 6-1
error = 3
alpha = 0.05
Z_half = qnorm(1-alpha/2)
sd2 = 10^2
n = Z_half^2 * sd2 / error^2 
round(n, 0)

# 6.2 - Estimating a Proportion for a Large Population --------------------
max_error = 0.03
p_hat = 0.8
n = Z_half^2 * p_hat * (1-p_hat) / max_error^2 
round(n, 0) # 683

# assume p_hat = 0.5
n = Z_half^2 * 0.5 * (1-0.5) / max_error^2 
round(n, 0) # 1067


# 6.3 - Estimating a Proportion for a Small, Finite Population ------------
N = 2000
max_error = 0.04
p_hat = 0.5
m = Z_half^2 * p_hat * (1-p_hat) / max_error^2 
m = ceiling(m)
n = m / (1 + (m-1)/N)
ceiling(n)
