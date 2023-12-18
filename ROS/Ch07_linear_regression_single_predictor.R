
# 7 Linear regression with a single predictor -----------------------------

library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")

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

# Plot regression line - simplified version
plot(c(-.7, 4.5), c(43,63), type="n",
     xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share",
     main="Data and linear fit", bty="l")
with(hibbs, points(growth, vote, pch=20))
abline(coef(M1), col="gray15")

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

# Point prediction given 2% growth
new <- data.frame(growth=2.0)
y_point_pred <- predict(M1, newdata=new) # 52.40971
print(y_point_pred)

# Uncertainty in prediction given 2% growth
y_linpred <- posterior_linpred(M1, newdata=new)

# Predictive uncertainty
y_pred <- posterior_predict(M1, newdata=new)

# Predictive uncertainty manually
sigma <- sims[,3]
n_sims <- nrow(sims)
y_pred <- a + b*as.numeric(new) + rnorm(n_sims, 0, sigma)

# Summarize predictions
Median <- median(y_pred)
MAD_SD <- mad(y_pred)
win_prob <- mean(y_pred > 50)
cat("Predicted Clinton percentage of 2-party vote: ", round(Median,1),
    ", with s.e. ", round(MAD_SD, 1), "\nPr (Clinton win) = ", round(win_prob, 2),
    sep="")

# Summarize predictions graphically
hist(y_pred)

# Comparison to lm()
M1a <- lm(vote ~ growth, data=hibbs)
summary(M1a)

# fit a line iwth zero intercept y = bx
M0 <- stan_glm(vote ~ -1 + growth, data = hibbs, refresh = 0)
print(M0)

# Checking using fake-data simulation -------------------------------------
# Step 1: Creating the pretend world
a <- 46.3
b <- 3.0
sigma <- 3.9
x <- hibbs$growth
n <- length(x)

# Step 2: Simulating fake data
y <- a + b*x + rnorm(n, 0, sigma)
fake <- data.frame(x, y)

# Step 3: Fitting the model and comparing fitted to assumed values
fit <- stan_glm(y ~ x, data = fake)
print(fit)

# check the coefficients
b_hat <- coef(fit)["x"]
b_se <- se(fit)["x"]
cover_95 <- abs(b-b_hat) < 2*b_se
cat(paste("95% coverage: ", cover_95))


# Formulating comparisons as regression models ----------------------------

# Estimating the mean is the same as regressing on a constant term
n_0 <- 20

# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2023)
y_0 <- rnorm(n_0, 2.0, 5.0)
fake_0 <- data.frame(y_0)
round(y_0, 1)

# check the mean and sd
round(mean(y_0), 2)
round(sd(y_0)/sqrt(n_0), 2)

# Estimating the mean is the same as regressing on a constant term
fit_2 <- stan_glm(y_0 ~ 1, data = fake_0, seed=2141, refresh = 0,
                  prior_intercept = NULL, prior = NULL, prior_aux = NULL)
print(fit_2)
