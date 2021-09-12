# Building a bayesian AB Test ---------------------------------------------
# Ref 15 FROM PARAMETER ESTIMATION TO HYPOTHESIS TESTING
n_trials = 100000
prior_alpha = 3
prior_beta = 7

a_samples = rbeta(n_trials, 36 + prior_alpha, 114 + prior_beta)
b_samples = rbeta(n_trials, 50 + prior_alpha, 100 + prior_beta)

prob_b_better = sum(b_samples > a_samples) / n_trials
prob_b_better # 0.96028

# histogram of b / a ratio
hist(b_samples / a_samples)
boxplot(b_samples / a_samples)

# CDF
plot(ecdf(b_samples / a_samples))
