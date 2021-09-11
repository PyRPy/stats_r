# beta PDF ----------------------------------------------------------------
# reference : Bayesian Statistics the Fun Way
# chapter 13
xs = seq(0.005, 0.01, by = 0.00001)
xs_all = seq(0, 1, by = 0.00001)

# beta distribution
prob_beta = dbeta(xs, 300, 40000 - 300)

plot(xs, prob_beta, 
     type = 'l',
     lwd = 3,
     ylab = 'density',
     xlab = "probability of subscription",
     main = "PDF Beta(300, 39700)")

# CDF for beta distribution
pbeta(0.0065, 300, 39700)
pbeta(0.0085, 300, 39700)

# quantiles
qbeta(0.999, 300, 39700)
