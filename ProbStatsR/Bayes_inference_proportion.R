# Inference for a Population Proportion -----------------------------------
# Ref : Applied Bayesian Statistics
# 90% CI for pi based on 7 success in 50 trials
binom.test(7, 50, conf.level = 0.9) # 0.0675967 0.2469352

# hypothesis about a population proportion: 
# test Ho: pi <=0.1 Ha: pi > 0.1
binom.test(7, 50, p = 0.1, alternative = "greater")


# Bayesian Posterior Intervals --------------------------------------------

# Equal-Tail Posterior Credible Sets
# prior beta(10, 40) , posterior beta(17, 83)
qbeta(c(0.025, 0.975), 17, 83)

# use a uniform prior, then beta(8, 44)
qbeta(c(0.025, 0.975), 8, 44) # wider than above


# Using the Posterior Distribution to Test Hypotheses ---------------------
# with prior beta(10, 40), posterior beta(17, 83), to get P(pi<=0.1|y)
pbeta(0.1, 17, 83)

pbeta(0.1, 8, 44)


# Posterior Predictive Distributions --------------------------------------
# posterior distribution beta(8, 44), new sample size n = 25
# the probability of getting 3, 4, 5, 6 'yes' as answers in 
# the future sample
library(LearnBayes)
pprobs = pbetap(c(8, 44), 25, 3:6)
round(cbind(3:6, pprobs), 4)
