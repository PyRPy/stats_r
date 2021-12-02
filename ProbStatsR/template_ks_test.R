# https://search.r-project.org/CRAN/refmans/dgof/html/ks.test.html
require(graphics)
require(dgof)

set.seed(1)

x <- rnorm(50)
y <- runif(30)
hist(x)
hist(y)

# Do x and y come from the same distribution?
ks.test(x, y)

# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

# test if x is stochastically larger than x2
x2 <- rnorm(50, -1)
plot(ecdf(x), xlim=range(c(x, x2)))
plot(ecdf(x2), add=TRUE, lty="dashed")
t.test(x, x2, alternative="g")

# Welch Two Sample t-test
# 
# data:  x and x2
# t = 4.3915, df = 97.784, p-value = 1.426e-05
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.5450745       Inf
# sample estimates:
#   mean of x   mean of y 
# -0.08862682 -0.96514122

wilcox.test(x, x2, alternative="g")
# Wilcoxon rank sum test with continuity correction
# 
# data:  x and x2
# W = 1847, p-value = 1.96e-05
# alternative hypothesis: true location shift is greater than 0


ks.test(x, x2, alternative="l")
# Two-sample Kolmogorov-Smirnov test
# 
# data:  x and x2
# D^- = 0.44, p-value = 6.252e-05
# alternative hypothesis: the CDF of x lies below that of y
#########################################################

# TBA, JWE new examples added for discrete distributions:

x3 <- sample(1:10, 25, replace=TRUE)

# Using ecdf() to specify a discrete distribution:
ks.test(x3, ecdf(1:10))

# Using step() to specify the same discrete distribution:
myfun <- stepfun(1:10, cumsum(c(0, rep(0.1, 10))))
ks.test(x3, myfun)

# The previous R ks.test() does not correctly calculate the
# test statistic for discrete distributions (gives warning):
# stats::ks.test(c(0, 1), ecdf(c(0, 1)))
# ks.test(c(0, 1), ecdf(c(0, 1)))

# Even when the correct test statistic is given, the
# previous R ks.test() gives conservative p-values:
stats::ks.test(rep(1, 3), ecdf(1:3))
ks.test(rep(1, 3), ecdf(1:3))
ks.test(rep(1, 3), ecdf(1:3), simulate=TRUE, B=10000)

# https://www.geeksforgeeks.org/kolmogorov-smirnov-test-in-r-programming/
