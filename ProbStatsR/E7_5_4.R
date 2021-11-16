
# Exercise 7.5-4  --------------------------------------------------------
# ordered stats

# read the data
dat <- scan('Data415/E7_5-04.txt')
hist(dat)

weight_in_order = sort(dat)
weight_in_order

# [1] 80.16 80.27 80.27 80.28 80.28 80.32 80.32 80.35 80.38
# [10] 80.40 80.51 80.53 80.56 80.59

median(weight_in_order) # 80.335

