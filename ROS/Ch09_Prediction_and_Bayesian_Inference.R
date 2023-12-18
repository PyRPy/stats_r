
# Chapter 9 Prediction and Bayesian inference -----------------------------

library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
# library(rosdata)
# data
hibbs <- read.table("ROS_Data/hibbs.dat", header=TRUE)
head(hibbs)

# Linear regression
# refresh = 0 supresses the default Stan sampling progress output
M1 <- stan_glm(vote ~ growth, data = hibbs, refresh = 0)

# summary
print(M1)
prior_summary(M1)
summary(M1) # MCMC diagnostics

# Posterior interval
round(posterior_interval(M1),1)


# 9.1 Propagating uncertainty in inference using posterior simulat --------

# Extract the simulations
sims <- as.matrix(M1)
a <- sims[,1]
b <- sims[,2]
sigma <- sims[,3]
n_sims <- nrow(sims)

# Median and mean absolute deviation (MAD_SD)
Median <- apply(sims, 2, median)
MAD_SD <- apply(sims, 2, mad)
print(cbind(Median, MAD_SD))

# estimate and standard error for some combination of parameters
a <- sims[,1]
b <- sims[,2]
z <- a/b
print(c(median(z), mad(z)))

# 9.2 Prediction and uncertainty ------------------------------------------
# Point prediction using predict
new <- data.frame(growth=2.0)
y_point_pred <- predict(M1, newdata=new)

# compute by hand
new <- 2.0
a_hat <- coef(M1)[1]
b_hat <- coef(M1)[2]
y_point_pred <- a_hat + b_hat*new # a single number

# Linear predictor with uncertainty
new <- data.frame(growth=2.0)
y_linpred <- posterior_linpred(M1, newdata=new)
# returns a vector of posterior simulations

hist(y_linpred)
mean(y_linpred)

# vector of simulations to compute by hand
sims <- as.matrix(M1)
a <- sims[,1]
b <- sims[,2]
y_linpred <- a + b*as.numeric(new) # also a vector of numbers
quantile(y_pred, c(0.025, 0.975)) # (44.0, 60.4) for 95% CI

# Predictive distribution for a new observation
y_pred <- posterior_predict(M1, newdata=new)
mean(y_pred)
quantile(y_pred, c(0.025, 0.975)) # (44.0, 60.4) for 95% CI

# with an error term
n_sims <- nrow(sims)
sigma <- sims[,3]
y_pred <- a + b*as.numeric(new) + rnorm(n_sims, 0, sigma)
quantile(y_pred, c(0.025, 0.975)) # (44.3, 60.6) for 95% CI

# Prediction given a range of input values
new_grid <- data.frame(growth=seq(-2.0, 4.0, 0.5))
y_pred_grid <- posterior_predict(M1, newdata=new_grid)
round(apply(y_pred_grid, 2, mean), 1)
#    1    2    3    4    5    6    7    8    9   10   11   12   13
# 40.1 41.7 43.2 44.7 46.2 47.7 49.3 50.8 52.4 53.9 55.3 57.1 58.5

# Propagating uncertainty
x_new <- rnorm(n_sims, 2.0, 0.3)
y_pred <- rnorm(n_sims, a + b*x_new, sigma)
quantile(y_pred, c(0.025, 0.975)) # (43.8, 61.1) wider 95% CI


# 9.3 Prior information and Bayesian synthesis ----------------------------
# prior vote = 52.4%, se = 4.1%
# data  vote from survey y = 190, n = 400
theta_hat_prior <- 0.524
se_prior <- 0.041
n <- 400
y <- 190
theta_hat_data <- y/n
se_data <- sqrt((y/n)*(1-y/n)/n)
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) /
  (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))
# vote 0.488, se = 0.021 based on bayes

# Weakly informative prior distribution based on subject-matter knowledge
# economic growth 0 to 4%
# b coef for growth to vote increase mean = 5, sd = 5
M4 <- stan_glm(vote ~ growth, data = hibbs,
               prior=normal(5,5),
               prior_intercept = normal(50, 10))
print(M4)

# Example where an informative prior makes a difference
load("ROS_Data/sexratio.rda")

# Fit a linear regression as default
fit_default <- stan_glm(y ~ x, data=sexratio)
print(fit_default)
#             Median MAD_SD
# (Intercept) 49.3    2.0
# x            1.4    1.4
#
# Auxiliary parameter(s):
#       Median MAD_SD
# sigma 4.6    1.7


# fit an informative-prior regression:
fit_post <- stan_glm(y ~ x, data=sexratio,
                     prior=normal(0, 0.2),
                     prior_intercept=normal(48.8, 0.5))
print(fit_post)
#             Median MAD_SD
# (Intercept) 48.8    0.5
# x            0.0    0.2
#
# Auxiliary parameter(s):
#       Median MAD_SD
# sigma 4.2    1.3
