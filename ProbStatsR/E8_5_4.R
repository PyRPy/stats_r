
# Exercise 8.5-4  --------------------------------------------------------
# ordered stats

# read the data
dat <- scan('Data415/E8_5-04.txt')
hist(dat)

# a) sign test
sample_in_order = sort(dat)
sample_in_order
# [1] -74.0216  -5.9848  -3.0678  -1.9415  -1.0962  -0.7757  -0.0790   0.5901
# [9]   3.8545   9.3820


median(sample_in_order)

# find how many numbers are more than 0
sum(dat > 0) # 3

# b) wilcoxon sign test
diff_abs = abs(dat - 0)
rank_obs = rank(diff_abs) # corrected to rank() from order()

W = -sum(rank_obs[1:7]) + sum(rank_obs[8:10]) # -9

n = length(dat)
Z = (W - 0)/sqrt(n * (n+1) * (2*n + 1)/6) # -0.9683

# use R base function
wilcox.test(dat, mu = 0, alternative = "two.sided")
# V = 18, p-value = 0.375
# alternative hypothesis: true location is not equal to 0

