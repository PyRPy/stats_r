
# Exercise 9.1-10  --------------------------------------------------------
# determine whether the data is a poisson distribution or not

# read the data
dat <- scan('Data415/E9_1-10.txt')
hist(dat)

# a) table the data
table_dat = table(dat)

#  0  1  2  3  4  5  6  7  8 
# 17 47 63 63 49 28 21 11  1

# b) find sample mean, and variance
mean(dat) # 3.03
var(dat)  # 3.19

# c) histogram comparison
dat_sim = dpois(0:8, 3)
hist(dat, freq = FALSE)
lines(dat_sim)

# d) chi-square goodness-of-fit test
counts = as.numeric(table_dat)

observed = counts
expected = length(dat) * dat_sim

q8 = sum((observed - expected)^2 / expected) # 7.403

qchisq(1-0.05, 8) # 

(q8 >= qchisq(1-0.05, 8)) # false

# do not reject Ho; conclude that the data comes from poisson distribution.


# correction on table cell values -----------------------------------------

counts = as.numeric(table_dat)
counts2 = counts[-9]
counts2[8] = 12
dat_sim2 = dpois(0:7, 3)
dat_sim2[8] = dat_sim2[8] + dpois(8, 3) # correct the last cell
observed2 = counts2
expected2 = length(dat) * dat_sim2

q7 = sum((observed2 - expected2)^2 / expected2) # 4.4809

qchisq(1-0.05, 7) # 

(q7 >= qchisq(1-0.05, 8)) # false
