
# How to Use the rstanarm Package -----------------------------------------
# https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html
# https://cran.r-project.org/web/packages/rstanarm/vignettes/aov.html

library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())


# Data --------------------------------------------------------------------

data("weightgain", package = "HSAUR3")
library(lattice)
xyplot(weightgain ~ type | source, data = weightgain,
       groups = source
)

xyplot(weightgain ~ type, data = weightgain
)

hist(weightgain$weightgain)
boxplot(weightgain ~ type, data = weightgain)
boxplot(weightgain ~ source, data = weightgain)

# Linear model ------------------------------------------------------------

lm1 <- lm(weightgain ~ source * type, data = weightgain)
anova(lm1)

# Analysis of Variance Table
# 
# Response: weightgain
#             Df Sum Sq Mean Sq F value  Pr(>F)  
# source       1  220.9  220.90  0.9879 0.32688  
# type         1 1299.6 1299.60  5.8123 0.02114 *
# source:type  1  883.6  883.60  3.9518 0.05447 .
# Residuals   36 8049.4  223.59 

coef(aov(weightgain ~ source * type, data = weightgain))
# (Intercept)         sourceCereal              typeLow 
# 100.0                -14.1                -20.8 
# sourceCereal:typeLow 
# 18.8 

# rstan based model -------------------------------------------------------
# need more efforts in interpretation
library(rstanarm)
post1 <- stan_aov(weightgain ~ source * type, data = weightgain, 
                  prior = R2(location = 0.5), adapt_delta = 0.999,
                  seed = 12345)
print(post1)

# ANOVA-like table:
#                     Median MAD_SD
# Mean Sq source      550.4  430.8 
# Mean Sq type        986.5  615.1 
# Mean Sq source:type 715.6  703.4 

