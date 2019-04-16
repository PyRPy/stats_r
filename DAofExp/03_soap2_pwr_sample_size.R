# soap2.r, soap experiment, Table 3.12, page 64

# install.packages("pwr")
library(pwr)
v = 3; del = 0.25; sig2 = 0.007; alpha = 0.05; pwr = 0.90
pwr.anova.test(k = v, sig.level = alpha, power = pwr,
               f = sqrt(del^2/(2*v*sig2)))

# del , difference
# v , number of levels of treatment factor
# sig2, largest value of error variance
# alpha, significance level
# pwr, power specified
