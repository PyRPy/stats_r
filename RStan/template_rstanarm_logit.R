
# How to Use the rstanarm Package -----------------------------------------
# https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html

library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())


# Data --------------------------------------------------------------------

data("womensrole", package = "HSAUR3")
womensrole$total <- womensrole$agree + womensrole$disagree


# Logistic regression -----------------------------------------------------
womensrole_glm_1 <- glm(cbind(agree, disagree) ~ education + gender,
                        data = womensrole, family = binomial(link = "logit"))
round(coef(summary(womensrole_glm_1)), 3)


# Rstan model -------------------------------------------------------------

library(rstanarm)
womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data = womensrole,
                              family = binomial(link = "logit"), 
                              prior = student_t(df = 7, 0, 5), 
                              prior_intercept = student_t(df = 7, 0, 5),
                              cores = 2, seed = 12345)
womensrole_bglm_1


# Intervals ---------------------------------------------------------------

ci95 <- posterior_interval(womensrole_bglm_1, prob = 0.95, pars = "education")
round(ci95, 2)

cbind(Median = coef(womensrole_bglm_1), MAD_SD = se(womensrole_bglm_1))

# Criticize the model -----------------------------------------------------
launch_shinystan(womensrole_bglm_1, ppd = FALSE)

y_rep <- posterior_predict(womensrole_bglm_1)
dim(y_rep)


# Update model ------------------------------------------------------------

(womensrole_bglm_2 <- update(womensrole_bglm_1, 
                             formula. = . ~ . + I(education^2)))


# leave one out model -----------------------------------------------------

loo_bglm_1 <- loo(womensrole_bglm_1)
loo_bglm_2 <- loo(womensrole_bglm_2)

loo_compare(loo_bglm_1, loo_bglm_2)


# Prediction on new data --------------------------------------------------

newdata <- data.frame(agree = c(0,0), disagree = c(100,100), 
                      education = c(12,16),
                      gender = factor("Female", levels = c("Male", "Female")))
y_rep <- posterior_predict(womensrole_bglm_2, newdata)
summary(apply(y_rep, 1, diff))
