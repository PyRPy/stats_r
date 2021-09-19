# bayes hypothesis test ---------------------------------------------------
# ref 19 FROM HYPOTHESIS TESTING TO PARAMETER ESTIMATION
pbinom(24, 100, 0.5)

# likelihood ratio for any two hypothesis
dx = 0.01
hypothesis = seq(0, 1, by=dx)

bayes_factor = function(h_top, h_bottom) {
  ((h_top)^24 * (1 - h_top)^76) / ((h_bottom)^24 * (1 - h_bottom)^76)
}

bfs = bayes_factor(hypothesis, 0.5)

plot(hypothesis, bfs, type = 'l')

# find largest Bayes factor
max(bfs)
hypothesis[which.max(bfs)] # 0.24

# vector of odds ratios
priors = ifelse(hypothesis >= 0.2 & hypothesis <= 0.3, 1/1000, 1)
plot(hypothesis, priors, type = 'l')

# find posteriors
posteriors = priors * bfs
plot(hypothesis, posteriors, type = 'l')

# summation of posterior odds and normalize
sum(posteriors)
p_posteriors = posteriors / sum(posteriors)
sum(p_posteriors)

plot(hypothesis, p_posteriors, type = 'l')

# the true rate of getting a prize is less than what the attendant claims
sum(p_posteriors[which(hypothesis < 0.5)])
# 0.9999995, the attendant is overstating the true prize rate !

# expectation
sum(p_posteriors * hypothesis)

# most likely estimate
hypothesis[which.max(p_posteriors)] # 0.19

# use beta distribution
xs_all = seq(0, 1, by = 0.01)

prob_beta = dbeta(xs_all, 24, 100-24)

plot(xs_all, prob_beta, 
     type = 'l',
     lwd = 3,
     ylab = 'density',
     xlab = "probability of subscription",
     main = "PDF Beta(24, 76)")

