
# Count Data with rstanarm ------------------------------------------------
# https://cran.r-project.org/web/packages/rstanarm/vignettes/count.html
library(ggplot2)
library(bayesplot)

# Poisson and Negative Binomial Regression Example
library(rstanarm)
data(roaches)
head(roaches)
#     y roach1 treatment senior exposure2
# 1 153 308.00         1      0  0.800000
# 2 127 331.25         1      0  0.600000
# 3   7   1.67         1      0  1.000000
# 4   7   3.00         1      0  1.000000
# 5   0   2.00         1      0  1.142857
# 6   0   0.00         1      0  1.000000

summary(roaches)

# Rescale
roaches$roach1 <- roaches$roach1 / 100

# Estimate original model
glm1 <- glm(y ~ roach1 + treatment + senior, offset = log(exposure2), 
            data = roaches, family = poisson)
summary(glm1)
#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  3.089246   0.021234  145.49   <2e-16 ***
#   roach1       0.698289   0.008874   78.69   <2e-16 ***
#   treatment   -0.516726   0.024739  -20.89   <2e-16 ***
#   senior      -0.379875   0.033418  -11.37   <2e-16 ***

# Estimate Bayesian version with stan_glm
stan_glm1 <- stan_glm(y ~ roach1 + treatment + senior, offset = log(exposure2),
                      data = roaches, family = poisson, 
                      prior = normal(0, 2.5), 
                      prior_intercept = normal(0, 5),
                      seed = 12345)
summary(stan_glm1)
#               mean   sd   10%   50%   90%
# (Intercept)  3.1    0.0  3.1   3.1   3.1 
# roach1       0.7    0.0  0.7   0.7   0.7 
# treatment   -0.5    0.0 -0.5  -0.5  -0.5 
# senior      -0.4    0.0 -0.4  -0.4  -0.3 

# point estimates and uncertainties
round(rbind(glm = coef(glm1), stan_glm = coef(stan_glm1)), digits = 2)

round(rbind(glm = summary(glm1)$coefficients[, "Std. Error"], 
            stan_glm = se(stan_glm1)), digits = 3)

# compare the observed data to replicated datasets
yrep <- posterior_predict(stan_glm1)
prop_zero <- function(y) mean(y == 0)
(prop_zero_test1 <- pp_check(stan_glm1, plotfun = "stat", 
                             stat = "prop_zero", binwidth = .005))
# not a good fit for the current poisson model


# negative binomial model  ------------------------------------------------
stan_glm2 <- update(stan_glm1, family = neg_binomial_2) 

prop_zero_test2 <- pp_check(stan_glm2, plotfun = "stat", stat = "prop_zero", 
                            binwidth = 0.01)
# Show graphs for Poisson and negative binomial side by side
bayesplot_grid(prop_zero_test1 + ggtitle("Poisson"), 
               prop_zero_test2 + ggtitle("Negative Binomial"), 
               grid_args = list(ncol = 2))
