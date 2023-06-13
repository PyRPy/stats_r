

# Simple linear regression using BUGS -------------------------------------
# 10.2 Introduction to Bayesian Simple Linear Regression
# http://rstudio-pubs-static.s3.amazonaws.com/1935_652d6fb17d7941c4b91bbbc5a22d4494.html

library(R2OpenBUGS)

# model
linear_model <- function() {

  for (i in 1:N) {
    # x is centered
    xcent[i] <- x[i] - mean(x[])
  }
  # iter index wrong, tau name wrong, y lower case wrong - DEBUG
  for (i in 1:N) {
    mu[i] <- beta0 + beta1 * xcent[i]
    y[i] ~ dnorm(mu[i], tausq)
  }

  # priors
  beta0 ~ dflat()
  beta1 ~ dflat()
  tausq ~ dgamma(0.001, 0.001)
  # regression standard deviation or std
  sigma <- 1 / sqrt(tausq)
}


# data
x = c(1997, 1998, 1999, 2000, 2001, 2002, 2003,
      2004, 2005, 2006, 2007, 2008, 2009, 2010)
y = c(2.2952, 2.3435, 2.5512, 2.5531, 2.3918, 2.1546, 2.3596,
      2.2431, 2.1725, 2.3162, 2.3504, 2.1926, 1.9638, 2.2025)
data_mercury <- list( x = x,
                      y = y,
                      N = 14)

# initials
inits_lm <- function() {
  list(beta0 = 0, beta1 = 0, tausq = 1)
}

# run BUGS model for sampling
model_output <- bugs(data=data_mercury,
                inits = inits_lm,
                parameters.to.save = c("beta0", "beta1", "sigma"),
                model.file = linear_model,
                n.chains = 1,
                n.iter = 3000,
                debug = TRUE)

# print out result
print(round(model_output$summary, 4))

#              mean     sd     2.5%      25%      50%      75%   97.5%
# beta0      2.2932 0.0382   2.2150   2.2700   2.2940   2.3160  2.3635
# beta1     -0.0233 0.0095  -0.0427  -0.0293  -0.0231  -0.0171 -0.0036
# sigma      0.1391 0.0327   0.0910   0.1174   0.1346   0.1548  0.2134
# deviance -16.4813 2.8495 -19.7052 -18.4825 -17.2350 -15.3000 -9.1032

# hypothesis test on Ho: beta1 = 0; Ha: beta1 != 0
# credible interval / set (-0.0427, -0.0036) does not include 0
# conclude that beta1 is not equal to zero, Ho is rejected


# prediction based on the linear model
linear_model_pred <- function() {

  for (i in 1:N) {
    # x is centered
    xcent[i] <- x[i] - mean(x[1:14])
  }
  # iter index wrong, tau name wrong, y lower case wrong - DEBUG
  for (i in 1:N) {
    mu[i] <- beta0 + beta1 * xcent[i]
    y[i] ~ dnorm(mu[i], tausq)
  }

  # priors
  beta0 ~ dflat()
  beta1 ~ dflat()
  tausq ~ dgamma(0.001, 0.001)
  # regression standard deviation or std
  sigma <- 1 / sqrt(tausq)
  # postprob <- step(beta1)
}

# data
x = c(1997, 1998, 1999, 2000, 2001, 2002, 2003,
      2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)
y = c(2.2952, 2.3435, 2.5512, 2.5531, 2.3918, 2.1546, 2.3596,
      2.2431, 2.1725, 2.3162, 2.3504, 2.1926, 1.9638, 2.2025, NA)
data_mercury <- list( x = x,
                      y = y,
                      N = 15)

# initials
inits_lm <- function() {
  list(beta0 = 0, beta1 = 0, tausq = 1)
}

# run BUGS model for sampling
model_output_pred <- bugs(data=data_mercury,
                     inits = inits_lm,
                     parameters.to.save = c("beta0", "beta1", "sigma", "y[15]"),
                     model.file = linear_model,
                     n.chains = 1,
                     n.iter = 3000,
                     debug = TRUE)

# print out result
print(round(model_output_pred$summary, 4))
# y prediction 95% CI ( 1.8045, 2.4270)
#              mean     sd     2.5%      25%      50%      75%   97.5%
# beta0      2.2810 0.0369   2.2050   2.2580   2.2820   2.3052  2.3535
# beta1     -0.0232 0.0093  -0.0419  -0.0292  -0.0232  -0.0171 -0.0056
# sigma      0.1377 0.0313   0.0916   0.1161   0.1330   0.1538  0.2114
# y[15]      2.1175 0.1576   1.8045   2.0140   2.1215   2.2230  2.4270
# deviance -16.5787 2.6944 -19.6753 -18.5200 -17.2600 -15.3975 -9.3165
