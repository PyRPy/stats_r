#BIOL933, Lab 6
#Example 2

#This script performs a full ANOVA on an RCBD
#read in, re-classify, and inspect the data
power_dat <- read.csv("Lab6ex2.csv")

power_dat<-as.data.frame(power_dat)
power_dat$trtmt<-as.factor(power_dat$trtmt)
str(power_dat, give.attr = F)

#The ANOVA
power_mod<-lm(response ~ trtmt, power_dat)
anova(power_mod)

# library(agricolae)
# tukey<-HSD.test(power_mod, "trtmt")

#TESTING ASSUMPTIONS
#Generate residual and predicted values
power_dat$resids <- residuals(power_mod)
power_dat$preds <- predict(power_mod)
power_dat$sq_preds <- power_dat$preds^2

#Look at a plot of residual vs. predicted values
plot(resids ~ preds, data = power_dat,
	xlab = "Predicted Values",
	ylab = "Residuals")

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(power_dat$resids)

#Perform Levene's Test for homogenity of variances
#install.packages("car")
library(car)
leveneTest(response ~ trtmt, data = power_dat, center = mean)
leveneTest(response ~ trtmt, data = power_dat, center = median)

# ----- Finding the exponent for a power transformation ----
  
means <- aggregate(power_dat$response, list(power_dat$trtmt), mean)
vars <- aggregate(power_dat$response, list(power_dat$trtmt), var)

logmeans <- log10(means$x)
logvars <- log10(vars$x)

power_mod<-lm(logvars ~ logmeans)
summary(power_mod)

#Create power-tranformed variable
# power = 1 - b/2
# b = coef of logmeans
# Power = 1 - (2.5814/2) = -0.29 
power_dat$trans_response<-(power_dat$response)^(-0.29)

#The ANOVA
trans_power_mod<-lm(trans_response ~ trtmt, power_dat)
anova(trans_power_mod)

# trans_tukey<-HSD.test(trans_power_mod, "trtmt")

#TESTING ASSUMPTIONS
#Generate residual and predicted values
power_dat$trans_resids <- residuals(trans_power_mod)
power_dat$trans_preds <- predict(trans_power_mod)

#Look at a plot of residual vs. predicted values
plot(trans_resids ~ trans_preds, data = power_dat,
     xlab = "Predicted Values",
     ylab = "Residuals")

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(power_dat$trans_resids)

#Perform Levene's Test for homogenity of variances
leveneTest(trans_response ~ trtmt, data = power_dat, center = mean)
leveneTest(trans_response ~ trtmt, data = power_dat, center = median)
