
# Exercise 8.5-3  --------------------------------------------------------
# ordered stats

# read the data
dat <- scan('Data415/E8_5-03.txt')
hist(dat)

# a) sign test
weight_in_order = sort(dat)
weight_in_order

# [1] 5.625 5.665 5.697 5.837 5.863 5.870 5.878 5.884 5.908
# [10] 5.967 6.019 6.020 6.029 6.032 6.037 6.045 6.049 6.050
# [19] 6.079 6.116 6.159 6.186 6.199 6.307 6.387

median(weight_in_order)

# find how many numbers are more than 5.9
sum(dat > 5.9) # 17 

# b) wilcoxon sign test
diff_abs = abs(dat - 5.9)
rank_obs = order(diff_abs)

W = -sum(rank_obs[1:8]) + sum(rank_obs[9:25])

n = length(dat)
Z = (W - 0)/sqrt(n * (n+1) * (2*n + 1)/6) # 2.758

# use t-test
mu = mean(dat)
x_sd = sd(dat)
t_stat = (mu - 5.9)/(x_sd/sqrt(n)) # 2.61, df = 24
