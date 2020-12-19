# example for equipment lifetime ------------------------------------------

library(survival)
tt <- c(32, 38, 48, 37, 23, 14, 12, 21)
cens <- c(0, 1, 1, 1, 1, 0, 1, 0)
Surv(tt, cens)
result.km <- survfit(Surv(tt, cens) ~ 1, conf.type="log-log")
summary(result.km)

# Kaplan-Meier plot
plot(result.km, conf.int=F, ylab="Survival probability",
     xlab="Time in days")
