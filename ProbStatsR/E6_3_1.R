
# Exercise 6.3-1  --------------------------------------------------------
# ordered stats

# read the data
dat <- scan('Data415/E6_3-01.txt')
hist(dat)

time_in_order = sort(dat)
n = length(dat)
p = seq(1, n, 1)
percentile_time = as.numeric(p / (n + 1))
# find ordered statistics
time_in_order

# find median and 80% percentile
median(time_in_order) # 146
percentile_time 
percentile_time <= 0.8

time_in_order[33]
quantile(time_in_order, probs = c(0.5, 0.8))

# find first quantile and third quantile of the sample
quantile(time_in_order, probs = c(0.25, 0.75))

# corrections based on textbook definitions (k = n + 1, k=40,  40 x 0.5 = 20th)
time_in_order[20]
time_in_order[32]
time_in_order[30]
