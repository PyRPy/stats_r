#BIOL933, Lab 5
#Example 1

#This script performs a full ANOVA on an RCBD

#Read in, re-classify, and inspect the data; inform R that Block and Temp are factors
firm_dat <- read.csv("Data/Lab5ex1.csv", stringsAsFactors=FALSE)
firm_dat<-as.data.frame(firm_dat)
firm_dat$Block<-as.factor(firm_dat$Block)
firm_dat$Temp<-as.factor(firm_dat$Temp)
str(firm_dat, give.attr=F)

# alway plot the data first, if you can
library(ggplot2)
ggplot(firm_dat, aes(x=Temp, y=Firm, color=Block))+
  geom_point()+
  geom_line(aes(group=Block))
  

#The ANOVA
firm_mod<-lm(Firm ~ Temp + Block, firm_dat)
anova(firm_mod)

#TESTING ASSUMPTIONS
#Generate residual and predicted values
firm_dat$resids <- residuals(firm_mod)
firm_dat$preds <- predict(firm_mod)
firm_dat$sq_preds <- firm_dat$preds^2
head(firm_dat)

#Look at a plot of residual vs. predicted values
plot(resids ~ preds, data = firm_dat,
	xlab = "Predicted Values",
	ylab = "Residuals")

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(firm_dat$resids)

#Perform Levene's Test for homogenity of variances
#install.packages("car")
library(car)
leveneTest(Firm ~ Temp, data = firm_dat)
leveneTest(Firm ~ Block, data = firm_dat) #Unnecessary, because not comparing Blocks

#Perform a Tukey 1-df Test for Non-additivity
firm_1df_mod<-lm(Firm ~ Temp + Block + sq_preds, firm_dat)
anova(firm_1df_mod)

boxplot(Firm ~ Temp, data = firm_dat,
	main = "Effect of storage temperature on mulberry firmness",
	xlab = "Storage Temperature (C)",
	ylab = "Firmness")


#The following script illustrates a trend analysis of this data
#Read in the data again, from scratch
firm_dat<-as.data.frame(firm_dat)
firm_dat$Block<-as.factor(firm_dat$Block)
firm_dat$Temp<-as.numeric(firm_dat$Temp)
str(firm_dat, give.attr=F)

Temp<-firm_dat$Temp
Temp2<-Temp^2
Temp3<-Temp^3
Temp4<-Temp^4

firm_trend_mod<-lm(Firm ~ Block + Temp + Temp2 + Temp3 + Temp4, firm_dat)
anova(firm_trend_mod)
summary(firm_trend_mod)

#From the summary, we find that the data is best fit by a cubic polynomial
#DO YOU SEE WHY?
#To find the coefficients for that polynomial, you could run the following reduced model:

firm_trend_final_mod<-lm(Firm ~ Block + Temp + Temp2 + Temp3, firm_dat)
anova(firm_trend_final_mod)
summary(firm_trend_final_mod)

#Or you could follow the strategy shown in the last lab:
lm(Firm ~ Block + poly(Temp,3,raw=TRUE), firm_dat)

#Either way, you'll find that the equation of the best-fit line relating Temperature to Firmness is:
#Firmness = 1.58289 + 1.17897 * Temp - 0.08276 * Temp^2 + 0.00142 * Temp^3

#DO YOU SEE WHERE THIS CAME FROM, IN EACH CASE?
