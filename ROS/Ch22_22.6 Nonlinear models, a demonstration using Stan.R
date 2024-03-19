
# 22.6 Nonlinear models, a demonstration using Stan -----------------------
# https://avehtari.github.io/ROS-Examples/Golf/golf.html
# golf example
# Fitting a nonlinear model using Stan

library("rstan")
library("rstanarm")
invlogit <- plogis

# Set up the data
golf <- read.table("ROS_Data/golf.txt", header=TRUE, skip=2)
golf$se <- with(golf, sqrt((y/n)*(1-y/n)/n))
r <- (1.68/2)/12
R <- (4.25/2)/12
golf_data <- list(x=golf$x, y=golf$y, n=golf$n, J=nrow(golf), r=r, R=R)

# Plot data
par(mar=c(3,3,2,1), mgp=c(1.7,.5,0), tck=-.02)
with(golf, {
  plot(x, y/n, xlim=c(0, 1.1*max(x)), ylim=c(0, 1.02),
       xaxs="i", yaxs="i", pch=20, bty="l",
       xlab="Distance from hole (feet)", ylab="Probability of success",
       main="Data on putts in pro golf")
  segments(x, y/n + se, x, y/n-se, lwd=.5)
  text(x + .4, y/n + se + .02, paste(y, "/", n,sep=""), cex=.6, col="gray40")
})

# Logistic regression model with rstanarm
fit1 <- stan_glm(cbind(y, n-y) ~ x, family=binomial(link="logit"), data=golf,
                 refresh = 0)

# Post-processing
a_hat <- fit1$coefficients[1]
b_hat <- fit1$coefficients[2]

# Plot logistic regression result
par(mar=c(3,3,2,1), mgp=c(1.7,.5,0), tck=-.02)
with(golf, {
  plot(x, y/n, xlim=c(0, 1.1*max(x)), ylim=c(0, 1.02),
       xaxs="i", yaxs="i", pch=20, bty="l",
       xlab="Distance from hole (feet)", ylab="Probability of success",
       main="Fitted logistic regression")
  segments(x, y/n + se, x, y/n-se, lwd=.5)
  curve(invlogit(a_hat + b_hat*x), from=0, to=1.1*max(x), add=TRUE)
  text(10.6, .57, paste("Logistic regression,\n    a = ",
                        round(a_hat, 2), ", b = ", round(b_hat, 2), sep=""))
})

# Logistic regression model with rstan
stanfile_golf_logistic <- root("Golf","golf_logistic.stan")
writeLines(readLines(stanfile_golf_logistic))
scode <- "
  data {
  int J;
  int n[J];
  vector[J] x;
  int y[J];
}
parameters {
  real a;
  real b;
}
model {
  y ~ binomial_logit(n, a + b*x);
}
"


fit_logistic <- stan(model_code = scode, data = golf_data,
                     refresh = 0)
print(fit_logistic)
#          mean se_mean   sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
# a        2.23    0.00 0.06     2.12     2.19     2.23     2.27     2.35  1233    1
# b       -0.26    0.00 0.01    -0.27    -0.26    -0.26    -0.25    -0.24  1152    1
# lp__ -3021.12    0.03 0.95 -3023.62 -3021.50 -3020.82 -3020.44 -3020.18  1311    1

# Post-processing
sims_logistic <- as.matrix(fit_logistic)
a_hat <- median(sims_logistic[,"a"])
b_hat <- median(sims_logistic[,"b"])

# Plot logistic regression result
par(mar=c(3,3,2,1), mgp=c(1.7,.5,0), tck=-.02)
with(golf, {
  plot(x, y/n, xlim=c(0, 1.1*max(x)), ylim=c(0, 1.02),
       xaxs="i", yaxs="i", pch=20, bty="l",
       xlab="Distance from hole (feet)", ylab="Probability of success",
       main="Fitted logistic regression")
  segments(x, y/n + se, x, y/n-se, lwd=.5)
  curve(invlogit(a_hat + b_hat*x), from=0, to=1.1*max(x), add=TRUE)
  text(10.6, .57, paste("Logistic regression,\n    a = ",
                        round(a_hat, 2), ", b = ", round(b_hat, 2), sep=""))
})
