# Paired t-Interval -------------------------------------------------------

# https://online.stat.psu.edu/stat415/lesson/3/3.1
# Example 3-2
unaffect <- c(1.94, 1.44, 1.56, 1.58, 2.06,
              1.66, 1.75, 1.77, 1.78, 1.92, 
              1.25, 1.93, 2.04, 1.62, 2.08)
affect <- c(1.27, 1.63, 1.47, 1.39, 1.93,
            1.26, 1.71, 1.67, 1.28, 1.85,
            1.02, 1.34, 2.02, 1.59, 1.97)

pair_diff <- unaffect - affect
n = length(pair_diff)
mu_diff <- mean(pair_diff)

sd_diff = sd(pair_diff)
alpha = 0.05
t_inter = qt(c(alpha/2, 1-alpha/2), n-1)
CI = c(mu_diff + t_inter * sd_diff/ sqrt(n) )

round(CI, 4)
