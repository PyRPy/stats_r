# 5.1 - One Proportion ----------------------------------------------------
# https://online.stat.psu.edu/stat415/lesson/5/5.1
# Example 5-1
n = 418
p_hat = 280 / n

sd_p = sqrt(p_hat*(1-p_hat)/n)
alpha = 0.05
Z_inter = qnorm(c(alpha/2, 1-alpha/2))
CI = c(p_hat+ Z_inter * sd_p)

round(CI, 3)
# 0.625 0.715


# 5.2 - Two Proportions ---------------------------------------------------
n1 = 62
n2 = 61
p_hat1 = 32/62 
p_hat2 = 19/61

sd_diff = sqrt(p_hat1*(1-p_hat1)/n1 + p_hat2*(1-p_hat2)/n2)
alpha = 0.05
Z_inter = qnorm(c(alpha/2, 1-alpha/2))
CI_diff = c(p_hat1-p_hat2 + Z_inter * sd_diff)

round(CI_diff, 3)
