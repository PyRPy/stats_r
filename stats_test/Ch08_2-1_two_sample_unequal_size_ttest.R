
# Two sample t test with different sample size ----------------------------
# Ho: mu_x - mu_y = 0
# Ha: mu_x < mu_y
# with alpha = 0.05

dat <- read.delim("Ex8_2-1.txt", header=FALSE)
dat$V4 <- as.factor(dat$V4)
head(dat)

x <- subset(dat, V4 == 'X')[, "V3"]
y <- subset(dat, V4 == 'Y')[, "V3"]

# check the variance
var.test(x, y) # p-value = 0.569

# t test for two samples
t.test(x, y, var.equal = TRUE, alternative = "less")

# t = -2.8112
# p-value = 0.005086
# conclusion: p-value < 0.05, Ho rejected; there is sufficient evidence
# to reject null hypothesis and show that mean of x is less than that of
# y at the significance level at alpha = 0.05
