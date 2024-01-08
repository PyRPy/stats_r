
# Model evaluation --------------------------------------------------------

# load packages
library("rstanarm")
library("ggplot2")
library("bayesplot")

# load data
unemp <- read.table("ROS_Data/unemp.txt", header=TRUE)
head(unemp)

# plot the unemployment rate
par(mar=c(3,3,1,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(unemp$year, unemp$y, type="l", ylab="Unemployment rate", xlab="Year", yaxs="i",
     ylim=c(0, max(unemp$y)*1.05), xaxt="n", yaxt="n", bty="l")
axis(1, seq(1950,2010,10), rep("",7))
axis(1, seq(1950,2010,20))
axis(2, seq(0,10), rep("",11))
axis(2, c(0,5,10), paste (c(0,5,10), "%", sep=""))

# Fit a 1st-order autogregression
n <- nrow(unemp)
unemp$y_lag <- c(NA, unemp$y[1:(n-1)])
fit_lag <- stan_glm(y ~ y_lag, data=unemp, refresh=0)
print(fit_lag, digits=2)

# Simulate replicated datasets using posterior predict
y_rep <- posterior_predict(fit_lag)
y_rep <- cbind(unemp$y[1], y_rep)
n_sims <- nrow(y_rep)

# Simulate replicated dataset "manually"
sims <- as.matrix(fit_lag)
n_sims <- nrow(sims)
y_rep <- array(NA, c(n_sims, n))
for (s in 1:n_sims){
  y_rep[s,1] <- unemp$y[1]
  for (t in 2:n){
    y_rep[s,t] <- sims[s,"(Intercept)"] + sims[s,"y_lag"] * y_rep[s,t-1] + rnorm(1, 0, sims[s,"sigma"])
  }
}

# Plot the simulated unemployment rate series
par(mar=c(1,1,3,.1), mgp=c(2,.5,0), tck=-.01)
par(mfrow=c(3,5))
for (s in sort(sample(n_sims, 15))){
  plot (unemp$year, y_rep[s,], type="l", ylab="", xlab="", yaxs="i",
        ylim=c(0, max(unemp$y)*1.05), xaxt="n", yaxt="n", bty="l", main=paste("Simulation", s))
  axis(1, seq(1950,2010,10), rep("",7))
  axis(2, seq(0,10), rep("",11))
}

# Numerical posterior predictive check
test <- function(y){
  n <- length(y)
  y_lag <- c(NA, y[1:(n-1)])
  y_lag_2 <- c(NA, NA, y[1:(n-2)])
  return(sum(sign(y-y_lag) != sign(y_lag-y_lag_2), na.rm = TRUE))
}

test_y <- test(unemp$y)
test_rep <- apply(y_rep, 1, test)
print(mean(test_rep > test_y))

# compare the number of switches in the original data and simulated data
print(test_y)
print(quantile(test_rep, c(.1,.5,.9)))

# Plot test statistic for data and histogram of test statistics for replications
ppc_stat(y=unemp$y, yrep=y_rep, stat=test, binwidth = 1) +
  scale_y_continuous(breaks=NULL)
