
# 6.4 MaximumLikelihoodEstimation -----------------------------------------
# 10th Edition
# Ex 6.4-6 
dat <- scan("Data-R/E6_4-06.txt")

mu = mean(dat)
mu

n = length(dat)
S2 <- var(dat)*(n-1)/n # different than sigma2
S2
