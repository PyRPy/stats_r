# One-Way MANOVA ----------------------------------------------------------
# https://livebook.manning.com/book/r-in-action-second-edition/chapter-9/1
library(MASS)
attach(UScereal)
shelf <- factor(shelf)
y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN=mean)


cov(y)


# Manova ------------------------------------------------------------------

fit <- manova(y ~ shelf)
summary(fit)

summary.aov(fit)	


# Check assumptions -------------------------------------------------------

center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y,center,cov)
coord <- qqplot(qchisq(ppoints(n),df=p),
                  d, main="Q-Q Plot Assessing Multivariate Normality",
                  ylab="Mahalanobis D2")
abline(a=0,b=1)
# identify(coord$x, coord$y, labels=row.names(UScereal))

library(mvoutlier)
outliers <- aq.plot(y)
outliers

library(rrcov)
Wilks.test(y,shelf,method="mcd") # can use summary(... test = ...)
