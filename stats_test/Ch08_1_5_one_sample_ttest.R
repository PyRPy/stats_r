
# One sample t test -------------------------------------------------------
# Ho : mu_W = 0
# Ha : mu_W > 0

# this example is a 'paired' test,
# 'W' is the difference before and after the heavy training
dat <- read.table("Ex8_1-5.txt", quote="\"", comment.char="")
colnames(dat) <- "W"

W_mean = mean(dat$W) # 0.07875

# ttest , tail test
t.test(dat$W, alternative = "greater", mu = 0) # p-value = 0.0719

# p-value = 0.0719 > 0.05
# Conclusion: There is no enough evidence to reject Ho, therefore the
# difference made was not clear or no improvement was made.

# change alpha = 0.1
t.test(dat$W, alternative = "greater", mu = 0, conf.level = 0.9)


# ttest , two-sided test
t.test(dat$W, alternative = "two.sided", mu = 0)

# 95 percent confidence interval: (-0.0288942  0.1863942)
# which includes the average value = 0.07875

