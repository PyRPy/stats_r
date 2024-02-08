
# 16.4 Interactions are harder to estimate than main effects --------------
library("rstanarm")
# the z-score has a mean of 2.8 and standard deviation of 1, and
# there’s an 80% chance that the z-score exceeds 1.96
pnorm(2.8,1.96,1)

# The probability of seeing a difference that is “statistically significant”
# at the 5% level is the probability that the z-score exceeds 1.96;
# that is, pnorm(1.4,1.96,1) = 0.29.
pnorm(1.4,1.96,1)

raw <- rnorm(1e6, 1.4, 1)
significant <- raw > 1.96
mean(raw[significant])

# Understanding the problem by simulating regressions in R
n <- 1000
sigma <- 10
y <- rnorm(n, 0, sigma)
x1 <- sample(c(-0.5,0.5), n, replace=TRUE)
x2 <- sample(c(-0.5,0.5), n, replace=TRUE)
fake <- data.frame(c(y,x1,x2))
fit_1 <- stan_glm(y ~ x1, data=fake)
fit_2 <- stan_glm(y ~ x1 + x2 + x1:x2, data=fake)
print(fit_1)
print(fit_2)

# make the predictors take on the values 0 and 1 rather than−0.5 and 0.5
x1 <- sample(c(0,1), n, replace=TRUE)
x2 <- sample(c(0,1), n, replace=TRUE)
fake <- data.frame(c(y,x1,x2))
fit_1 <- stan_glm(y ~ x1, data=fake)
fit_2 <- stan_glm(y ~ x1 + x2 + x1:x2, data=fake)
print(fit_1)
print(fit_2)

# The standard error for the interaction is still 1.3, but the standard
# errors for the main effects went up to 0.9.

# What about coding each predictor as −1, 1?
x1 <- sample(c(-1,1), n, replace=TRUE)
x2 <- sample(c(-1,1), n, replace=TRUE)
fake <- data.frame(c(y,x1,x2))
fit_1 <- stan_glm(y ~ x1, data=fake)
fit_2 <- stan_glm(y ~ x1 + x2 + x1:x2, data=fake)
print(fit_1)
print(fit_2)

# the standard errors for the main effects are smaller by a factor of 2,
# and now the standard error for the interaction has been divided by 4.
