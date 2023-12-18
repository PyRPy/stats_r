
# Chapter 8 Fitting regression models -------------------------------------
# Confidence intervals, uncertainty intervals, compatibility intervals

x <- 1:10
y <- c(1,1,2,3,5,8,13,21,34,55)
fake <- data.frame(x, y)
fit <- stan_glm(y ~ x, data=fake)
print(fit)

# extract simulations
sims <- as.matrix(fit)

# extract 95% interval for coefficient for x
quantile(sims[,2], c(0.025, 0.975))
#       2.5%    97.5%
#   2.710582 7.497539

c(5.1 - 2*1.1, 5.1 + 2*1.1) # 2.9 7.3
