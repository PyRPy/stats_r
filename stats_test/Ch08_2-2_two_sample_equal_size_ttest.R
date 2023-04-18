
# Two sample t test with equal sample size ----------------------------
# Ho: mu_x - mu_y = 0
# Ha: mu_x - mu_y != 0
# with alpha = 0.05

dat <- read.delim("Ex8_2-2.txt", header=FALSE)
dat$V4 <- as.factor(dat$V4)
head(dat)

x <- subset(dat, V4 == 'X')[, "V3"]
y <- subset(dat, V4 == 'Y')[, "V3"]

# check the variance
var.test(x, y) # p-value = 0.8584

# t test for two samples
t.test(x, y, var.equal = TRUE, alternative = "two.sided")

# t = 2.053
# p-value = 0.05215
# conclusion: p-value > 0.05,
# Ho is not rejected; there is no sufficient evidence
# to reject null hypothesis and show that mean of x is no different than
# y at the significance level at alpha = 0.05

# visual observations
boxplot(x, y, names = c('x', 'y'))
