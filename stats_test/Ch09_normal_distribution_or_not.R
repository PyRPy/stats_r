
# create data from a normal distribution ----------------------------------
set.seed(2023)
dat <- rnorm(200, 0, 1) # mean = 0, std = 1

# check histrogram
hist(dat)

# check qqplot
qqnorm(dat, main='normal distribution or not')
qqline(dat)

# shapiro test
shapiro.test(dat)

# Kolmogorov-Smirnov Test
ks.test(dat, 'pnorm') # sensitive to sample size
