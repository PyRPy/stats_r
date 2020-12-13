# beansoaking.r, bean-soaking experiment, Table 8.10 p278, and code from text p280

# Table 8.10, p278
bean.data <- read.table("Data/bean.txt", header=T)
head(bean.data) 
dim(bean.data) 
plot(Length ~ x, data=bean.data)

# use ggplot for categorical factors
library(tidyverse)
bean.data %>% ggplot(aes(x=x, y=Length)) +
  geom_point()+
  geom_smooth()

# Fit regression models and generate ANOVA info
model3 <- lm(Length ~ x + I(x^2) + I(x^3), data=bean.data) # Fit cubic model
summary(model3) # Display least squares estimates, overall F test
anova(model3) # Display type 1 SS

# Would a lower order model suffice?
model2 <- lm(Length ~ x + I(x^2), data=bean.data) # Fit quadratic model
model1 <- lm(Length ~ x, data=bean.data) # Fit simple linear reg model
anova(model2, model3) # Can cubic term be dropped?
anova(model1, model3) # Can both cubic and quadratic terms be dropped?

# Compute predicted values, CIs, PIs, and std errors for x=8:34
# Set up a grid of x's for prediction: x=8:34
xPred <- data.frame(x=seq(8, 34, 1))

# Calculute fitted values, 95% CIs for mean response, se.fit
preds <- predict(model3, xPred, se.fit=T, interval=c("confidence"))

# Calculate 95% PIs for new observations 
preds2 <- predict(model3, xPred, interval = c("prediction"))

# preds; preds2 # (Reader: display preds and preds2 to see contents)
se.fit <- preds$se.fit # to remove "preds$" from column header name

# Compute standard error for prediction
rmse <- preds$residual.scale # used to compute se.pred
se.pred <- sqrt(preds$se.fit^2 + rmse^2)

# Consolidate results for display
stats <- cbind(xPred, preds$fit, se.fit, preds2[,2:3], se.pred)
head(stats) # display first six rows of results

# Plot data (length vs x), plus fitted model for x=8:34
plot(Length ~ x, xlim = c(8, 34), ylim = c(-10, 30), data=bean.data)
points(xPred$x, preds$fit[, 1], pch = 19)

# Some plots to check model assumptions
bean.data$e <- residuals(model3); # Obtain residuals 
bean.data$pred <- fitted(model3) # Obtain predicted values

# Plot residuals vs x
plot(e ~ x, ylab = "Residuals", las=1, xaxt="n", data=bean.data)
axis(1, at = c(12,18,24,30)); abline(h=0)

# Plot residuals vs predicted values
plot(e ~ pred, xlim=c(5,25), las=1, xaxt="n", data=bean.data, 
     xlab="Predicted Values", ylab = "Residuals")
axis(1, at=seq(5,25,5)); abline(h=0) 

# Normal probability plot of residuals
qqnorm(model3$res, ylim=c(-10,10), xlim=c(-4,4)); abline(h=0, v=0)

# Code from text p280
# Generate lack-of-fit test
bean.data$fA <- factor(bean.data$x)
modelA <- lm(Length ~ fA, data=bean.data)
anova(model2, modelA)

# pairs comparison
modelfA <- aov(Length ~ fA, data=bean.data)
TukeyHSD(modelfA, "fA", ordered = TRUE)
