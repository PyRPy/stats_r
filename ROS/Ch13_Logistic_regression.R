
#  13. LOGISTIC REGRESSION ------------------------------------------------

# Load packages

library("arm")
library("rstanarm")
library("foreign")

# Load data
nes <- read.table("ROS_Data/nes.txt", header=TRUE)
head(nes)
colnames(nes)

# Use first only data from 1992 and remove missing data
ok <- nes$year==1992 & !is.na(nes$rvote) & !is.na(nes$dvote) & (nes$rvote==1 | nes$dvote==1)
nes92 <- nes[ok,]

# Logistic regression of vote preference on income
fit_1 <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=nes92,
                  refresh=0)
print(fit_1)

# Predictions
new <- data.frame(income=5)
pred <- predict(fit_1, type="response", newdata=new)
print(pred, digits=2)

# Linear predictor with uncertainty
linpred <- posterior_linpred(fit_1, newdata=new)
head(linpred)

# mean value and sd
print(c(mean(linpred), sd(linpred)), digits=2)

# Expected outcome with uncertainty
epred <- posterior_epred(fit_1, newdata=new)
head(epred)
print(c(mean(epred), sd(epred)), digits=2)

# Predictive distribution for a new observation
postpred <- posterior_predict(fit_1, newdata=new)
head(postpred)
print(c(mean(postpred), sd(postpred)), digits=2)

