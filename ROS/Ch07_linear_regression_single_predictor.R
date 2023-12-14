
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
