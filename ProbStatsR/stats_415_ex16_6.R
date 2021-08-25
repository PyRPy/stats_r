# 16.3 - Unspecified Probabilities ----------------------------------------
# https://online.stat.psu.edu/stat415/lesson/16/16.3#toggleSidebar

X0 <- seq(1:12)
counts0 = c(1, 4, 13, 19, 16, 15, 9, 12, 7, 2, 1, 1)
X0_mean = sum(X0*counts0)/sum(counts0)

X <- seq(1, 9)
counts <- c(5, 13, 19, 16, 15, 9, 12, 7, 4)

X_mean = sum(X * counts) / sum(counts)

probi <- rep(0, 9)
probi[1] = dpois(0, X0_mean) + dpois(1, X0_mean) + dpois(2, X0_mean)
probi
probi[2:8] = dpois(seq(3, 9), X0_mean)
probi[9] = dpois(10, X0_mean) + dpois(11, X0_mean) + dpois(12, X0_mean)
round(probi, 4)
# 0.0830 0.1087 0.1520 0.1699 0.1583 0.1264 0.0883 0.0549 0.0535
Expd <- probi * 100

# chi-square test statistic
Q_stat = sum((counts - Expd)^2 / Expd)

# critical region
region_critical = qchisq(1-0.05, 8-1) # 14.06714

# reject Ho or not
ifelse(Q_stat > region_critical, "reject Ho", "fail to reject Ho")

