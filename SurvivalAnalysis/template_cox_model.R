# cox model ---------------------------------------------------------------

library(survival)
library(survminer)

data(GBSG2, package = "TH.data")
coxmodel <- coxph(Surv(time, cens) ~ horTh, data = GBSG2)

coef(coxmodel)

# weibull model
weibullmodel <- survreg(Surv(time, cens) ~ horTh, data = GBSG2)
coef(weibullmodel)

# visualization
coxmodel <- coxph(Surv(time, cens) ~ horTh + tsize, data = GBSG2)
# step 1
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75))
)

rownames(newdat) <- letters[1:6]
newdat

# step 2 compute survival curves
coxsf <- survfit(coxmodel, 
                 data = GBSG2, 
                 newdata = newdat, 
                 conf.type = "none")
str(coxsf)

# step 3 create data frame
surv_coxmodel0 <- surv_summary(coxsf)
head(surv_coxmodel0)
surv_coxmodel <- cbind(surv_coxmodel0,
                       newdat[as.character(surv_coxmodel0$strata), ])

# step 4 plot
ggsurvplot_df(surv_coxmodel, linetype = "horTh", color = "tsize",
              legend.title = NULL, censor = FALSE)


# ref DataCamp -Heidi Seibold ---------------------------------------------


