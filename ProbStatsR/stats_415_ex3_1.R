# Two-Sample Pooled t-Interval

# Two-Sample Pooled t-Interval --------------------------------------------

# https://online.stat.psu.edu/stat415/lesson/3/3.1
# Example 3-1
dinopis <- c(12.9, 10.2, 7.4, 7.0, 10.5, 11.9, 7.1, 9.9, 14.4, 11.3)
menneus <- c(10.2, 6.9, 10.9, 11.0, 10.1, 5.3, 7.5, 10.3, 9.2, 8.8)
n = length(dinopis)
m = length(menneus)
sd1 = sd(dinopis)
sd2 <- sd(menneus)

sp2 <- ((n-1)*sd1^2 + (m-1)*sd2^2)/(n + m - 2)
mu_diff = mean(dinopis) - mean(menneus)

alpha = 0.05
t_inter = qt(c(alpha/2, 1-alpha/2), n+m-2)
CI = c(mu_diff + t_inter * sqrt(sp2)*sqrt(1/n + 1/m))

round(CI, 3)
# -0.852  3.332


# Welch's t-Interval ------------------------------------------------------

# find degree of freedom
r = (sd1^2/n + sd2^2/m)^2 / ((sd1^2/n)^2/(n-1) + (sd2^2/m)^2/(m-1))
t_welch = qt(c(alpha/2, 1-alpha/2), floor(r))
CI_welch = c(mu_diff + t_welch * sqrt(sd1^2/n + sd2^2/m))
round(CI_welch, 3)
