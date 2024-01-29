
# 14.3 Predictive simulation ----------------------------------------------

library("arm")
library("rstanarm")
library("foreign")
library("loo")
invlogit <- plogis

# data
wells <- read.csv("ROS_Data/wells.csv")
head(wells)

n <- nrow(wells)

# Predict switching with distance, arsenic, and education
fit_7 <- stan_glm(switch ~ dist100 + arsenic + educ4,
                  family = binomial(link="logit"), data = wells)
print(fit_7, digits=2)

# stan_glm
# family:       binomial [logit]
# formula:      switch ~ dist100 + arsenic + educ4
# observations: 3020
# predictors:   4
# ------
#             Median MAD_SD
# (Intercept) -0.22   0.09
# dist100     -0.90   0.10
# arsenic      0.47   0.04
# educ4        0.17   0.04

# comparing households that are next to, or 100 meters from, the nearest safe well
b <- coef(fit_7)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*wells$arsenic + b[4]*wells$educ4) -
  invlogit (b[1] + b[2]*lo + b[3]*wells$arsenic + b[4]*wells$educ4)
round(mean(delta), 2)

# comparing households with existing arsenic levels of 0.5 and 1.0
b <- coef(fit_7)
hi <- 1.0
lo <- 0.5
delta <- invlogit (b[1] + b[2]*wells$dist100 + b[3]*hi + b[4]*wells$educ4) -
  invlogit (b[1] + b[2]*wells$dist100 + b[3]*lo + b[4]*wells$educ4)
round(mean(delta), 2)

# comparing householders with 0 and 12 years of education
b <- coef(fit_7)
hi <- 3
lo <- 0
delta <- invlogit (b[1]+b[2]*wells$dist100+b[3]*wells$arsenic+b[4]*hi) -
  invlogit (b[1]+b[2]*wells$dist100+b[3]*wells$arsenic+b[4]*lo)
round(mean(delta), 2)

# Predict switching with distance, arsenic, education and interactions
wells$c_dist100 <- wells$dist100 - mean(wells$dist100)
wells$c_arsenic <- wells$arsenic - mean(wells$arsenic)
wells$c_educ4 <- wells$educ4 - mean(wells$educ4)
fit_8 <- stan_glm(switch ~ c_dist100 + c_arsenic + c_educ4 +
                    c_dist100:c_educ4 + c_arsenic:c_educ4,
                  family = binomial(link="logit"), data = wells)

print(fit_8, digits=2)
# stan_glm
# family:       binomial [logit]
# formula:      switch ~ c_dist100 + c_arsenic + c_educ4 + c_dist100:c_educ4 +
#   c_arsenic:c_educ4
# observations: 3020
# predictors:   6
# ------
#                   Median MAD_SD
# (Intercept)        0.35   0.04
# c_dist100         -0.92   0.11
# c_arsenic          0.49   0.04
# c_educ4            0.19   0.04
# c_dist100:c_educ4  0.33   0.11
# c_arsenic:c_educ4  0.08   0.04

# comparing households that are next to, or 100 meters from, the nearest safe well
# with interactions effect included
b <- coef(fit_8)
hi <- 1
lo <- 0
delta <- invlogit(b[1] + b[2]*hi + b[3]*wells$c_arsenic +
                    b[4]*wells$c_educ4 + b[5]*hi*wells$c_educ4 +
                    b[6]*wells$c_arsenic*wells$c_educ4) -
  invlogit(b[1] + b[2]*lo + b[3]*wells$c_arsenic +
             b[4]*wells$c_educ4 + b[5]*lo*wells$c_educ4 +
             b[6]*wells$c_arsenic*wells$c_educ4)
round(mean(delta), 2)
