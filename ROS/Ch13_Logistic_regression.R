
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
