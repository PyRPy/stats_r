# Chapter 4 Example 1 Apple juice -----------------------------------------
# https://online.stat.psu.edu/stat506/node/20/
# read data
Apple <- read.delim("Apple.txt") 
head(Apple)
with(data = Apple, plot(X, Y))

# please change the code to your local address where you save the file 
# hint: use "/" if "\" shows in the local address and does not work.
attach(Apple)
plot(X, Y, main="Scatterplot of Y vs X", 
           xlab="X = Apple weight",
           ylab="Y = Apple juice's weight", 
           pch=19)

# check linear relationship and whether pass through origin
r_est_check<-lm(Y~X)
summary(r_est_check)

# Descriptive summary
summary(Apple)

# Standard Deviation
sd(X)
sd(Y)

# Calculate r
r <- mean(Y)/mean(X)
r

# mean estimation
mu_hat_r <- r*mean(X)
mu_hat_r

# total estimation
tau_x = 2000
tau_hat_r = r * tau_x
tau_hat_r

# Variance for mean
n = 15
s_sq_r <- (1/(n-1)) * sum((Y-r*X)^2) # follow the definition
s_sq_r

# Variance for total
Var_tau_r = tau_x/mean(X)*(tau_x/mean(X)-n)*s_sq_r/n
Var_tau_r

# Standard deviation for total
SD_tau_r = Var_tau_r^0.5
SD_tau_r

# 95% confidence interval for the total estimation 
CI95 = tau_hat_r+SD_tau_r*qt(c(.025,.975),14)
CI95

detach(Apple)
