
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

# Series of regressions for different years
yrs <- seq(1952, 2000, 4)
n_yrs <- length(yrs)
fits <- array(NA, c(n_yrs, 3), dimnames <- list(yrs, c("year", "coef", "se")))

for (j in 1:n_yrs){
  yr <- yrs[j]
  ok <- (nes$year==yr & !is.na(nes$presvote) & nes$presvote<3 &
           !is.na(nes$vote) & !is.na(nes$income))
  vote <- nes$presvote[ok] - 1
  income <- nes$income[ok]
  fit_y <- stan_glm(vote ~ income, family=binomial(link="logit"),
                    data = data.frame(vote, income),
                    warmup = 500, iter = 1500, refresh = 0,
                    save_warmup = FALSE, cores = 1, open_progress = FALSE)
  fits[j,] <- c(yr, coef(fit_y)[2], se(fit_y)[2])
}

# Plot the series of regression
par(mar=c(3,2.5,1,.2), tck=-.01, mgp=c(1.5, .3, 0))
plot (fits[,"year"], fits[,"coef"], xlim=c(1950,2000), ylim=range(fits[,"coef"]-fits[,"se"], fits[,"coef"]+fits[,"se"]),
      pch=20, ylab="Coefficient of income", xlab="Year", bty="l")
for (j in 1:n_yrs){
  lines(rep(fits[j,"year"], 2), fits[j,"coef"] + fits[j,"se"]*c(-1,1), lwd=.5)
}
abline(0,0,lwd=.5, lty=2)

# Estimate the with-in sample predictive accuracy
predp <- fitted(fit_1)
round(c(mean(predp[nes92$rvote==1]), mean(1-predp[nes92$rvote==0])), 3)

# predictive performance of a model using within-sample log-score
round(sum(log(c(predp[nes92$rvote==1], 1-predp[nes92$rvote==0]))), 1)

#  leave-one-out log-score
loo(fit_1)


# Wells switch decision ---------------------------------------------------

# data
wells <- read.csv("ROS_Data/wells.csv")
head(wells)

# Log-score for coin flipping
prob <- 0.5
round(log(prob)*sum(wells$switch) + log(1-prob)*sum(1-wells$switch),1)

# Log-score for intercept model
round(prob <- mean(wells$switch),2)
round(log(prob)*sum(wells$switch) + log(1-prob)*sum(1-wells$switch),1)

# Fit a model using distance to the nearest safe well
fit_1 <- stan_glm(switch ~ dist, family = binomial(link = "logit"), data=wells)
print(fit_1, digits=3)

# LOO log score
(loo1 <- loo(fit_1))

# Scale distance in meters to distance in 100 meters
wells$dist100 <- wells$dist/100

# Fit a model using scaled distance to the nearest safe well
fit_2 <- stan_glm(switch ~ dist100, family = binomial(link = "logit"), data=wells)
print(fit_2, digits=2)
(loo2 <- loo(fit_2, save_psis = TRUE))

# Two predictors
# Fit a model using scaled distance and arsenic level
fit_3 <- stan_glm(switch ~ dist100 + arsenic, family = binomial(link = "logit"),
                  data=wells)
(loo3 <- loo(fit_3, save_psis = TRUE))

# Compare models
loo_compare(loo2, loo3)

# Average improvement in LOO predictive probabilities
# from dist100 to dist100 + arsenic
pred2 <- loo_predict(fit_2, psis_object = loo2$psis_object)$value
pred3 <- loo_predict(fit_3, psis_object = loo3$psis_object)$value
round(mean(c(pred3[wells$switch==1]-pred2[wells$switch==1],pred2[wells$switch==0]-pred3[wells$switch==0])),3)
