# Implementation of a Survival Analysis in R ------------------------------

# packages
library(survival)
library(survminer)
library(dplyr)

# import data
data("ovarian")
glimpse(ovarian)
head(ovarian)

# futime is the response variable

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age) 

ovarian <- ovarian %>% mutate(age_group = ifelse(age >= 50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 

fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

# plot for comparison
ggsurvplot(fit1, data = ovarian, pval = TRUE) # pval = T for log rank test

# vertical lines are censored data

# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

# above regression is on residual disease status

## Cox proportional hazards models allow you to include covariates

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)
