# find CI
# https://online.stat.psu.edu/stat415/lesson/2/2.5
# Example 2-2
beef <- c(118, 115, 125, 110, 112,
          130, 117, 112, 115, 120, 
          113, 118, 119, 122, 123, 
          126)
n = length(beef)
mu = mean(beef)
sd_sample <- sd(beef)
alpha = 0.05
t_inter = qt(c(alpha/2, 1-alpha/2), n-1)
CI = c(mu + t_inter * sd_sample / sqrt(n) )

round(CI, 2)
# 115.42 121.45