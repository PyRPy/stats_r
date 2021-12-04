
# Lesson 22: Kolmogorov-Smirnov Goodness-of-Fit Test ----------------------
# Example 22-1 
x = c(0, 1, 2, 2, 4, 6, 6, 7)
ecdf(x)
plot(ecdf(x))

# Example 22-2
x = c(1.41, 0.26, 1.97, 0.33, 0.55, 0.77, 1.46, 1.18)
ks.test(x, "punif", 0, 2) # two-sided

# One-sample Kolmogorov-Smirnov test
# 
# data:  x
# D = 0.145, p-value = 0.9863
# alternative hypothesis: two-sided

# Example 22-3 Section
x = c(108, 112, 117, 130, 111, 131, 113, 113, 105, 128)
ks.test(x, "pnorm", 120, 10)

# One-sample Kolmogorov-Smirnov test
# 
# data:  x
# D = 0.35804, p-value = 0.154
# alternative hypothesis: two-sided
# 
# Warning message:
#   In ks.test(x, "pnorm", 120, 10) :
#   default ks.test() cannot compute correct p-values with ties;
# see help page for one-sample Kolmogorov test for discrete distributions.