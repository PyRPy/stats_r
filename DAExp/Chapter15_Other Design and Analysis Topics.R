
# Chapter15_Other Design and Analysis Topics.R ----------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)

# Box-Cox transformation
head(Example3.5)

model <- lm(Observation ~ EstimationMethod, data=Example3.5)
vals <- boxcox(model)
best.lambda <- vals$x[which.max(vals$y)]
print(best.lambda)

#  coupon redemption
head(Table15.1)
# 2 level interactions
model <- glm(cbind(Coupons, I(1000- Coupons)) ~ (A + B + C)^2,
             data=Table15.1, family='binomial')
print(summary(model))

# ratio of odds for factor A
exp(2*0.169208) # 1.402724


# grill defects experiment, poisson regression
head(Problem8.51)

df <- Problem8.51[,c('A','B','C','D','E','F','G','H','J')]
df[,'Defects'] <- round(Problem8.51[,'Sqrt']^2)

#  poisson model
model <- glm(Defects ~ D + F + B:G, data=df, family='poisson')
summary(model)

# 95% CI
response <- predict(model, type='response', se.fit=TRUE)
2 * qnorm(0.975) * response$se.fit

#  Worsted Yarn experiment, cycle time until failure
head(Table15.4)
model <- glm(CyclesToFailure ~ x1 + x2 + x3, data=Table15.4,
             family=Gamma(link='log'))
summary(model)
