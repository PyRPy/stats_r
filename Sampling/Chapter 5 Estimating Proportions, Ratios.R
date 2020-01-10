# Chapter 5 Estimating Proportions, Ratios, and Subpopulation Means
# http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-proportion
library(MASS)
head(survey)
tail(survey)
summary(survey)
colnames(survey)

# estimate proportion
gender.response <- na.omit(survey$Sex)
n <- length(gender.response)
k <- sum(gender.response == "Female")
pbar <- k / n; pbar

# standard error
SE <- sqrt(pbar * (1 - pbar)/n)
SE

# margin of error
E <- qnorm(0.975) * SE; E

# sample proportion with CI
pbar + c(-E, E)

# use prop.test function
prop.test(k, n)


# http://www.r-tutor.com/elementary-statistics/interval-estimation/sampling-size-population-proportion
# https://www.rdocumentation.org/packages/samplingbook/versions/1.2.2/topics/sample.size.prop
