#BIOL933
#Lab 3
#Example 1

#This script performs a CRD analysis (one-way ANOVA) and tests for homogeneity of variance

# Read data first
clover_dat <- read.csv("Data/Lab3ex1.csv")

#Load the data, then use the following function to examine the structure of your data
str(clover_dat, give.attr = F)

#Convert clover_dat to a dataframe and tell R that "Culture" is a classification variable; re-examine
clover_dat <- as.data.frame(clover_dat)
clover_dat$Culture <- as.factor(clover_dat$Culture)
str(clover_dat, give.attr = F)

#Apply the linear model and look at the results (ANOVA table)
clover_mod<-lm(NLevel ~ Culture, data = clover_dat)
anova(clover_mod)

#Some more results
summary(clover_mod)

#Create box-and-whisker plot of the data
plot(clover_dat)


#Testing for homogeneity of variances using Levene's original test

# how about plot the model
par(mfrow=c(2,2))
plot(clover_mod)
dev.off()

#extract residual values
clover_res<-residuals(clover_mod)
clover_res

#add these residual values to the original dataframe
clover_dat$res<-clover_res
clover_dat$absres<-abs(clover_res) #for Levene's original test, use the absolute values of residuals
clover_dat$res2<-clover_res^2 #for a more robust test, use squared residuals
head(clover_dat)  # Use head() to look at the first six lines of a dataframe

#Test for homogeneity of variances
leveneABS_mod<-lm(absres ~ Culture, data = clover_dat)
anova(leveneABS_mod)

leveneRES2_mod<-lm(res2 ~ Culture, data = clover_dat)
anova(leveneRES2_mod)


#The Levene Test is available through the "car" package
#install.packages("car")
library(car)

leveneTest(clover_mod, center = mean) #same as abs(res)
leveneTest(clover_mod, center = median) #the default


#alternative HOV tests
bartlett.test(NLevel ~ Culture, data = clover_dat)  

#parametric test; very similar to res^2
#sensitive to departures from normality

fligner.test(NLevel ~ Culture, data = clover_dat)  #non-parametric test


#For the interested:
#Testing for homogeneity of variances using Brown and Forsythe's improved Levene's Test,
#based on an ANOVA of the absolute values of median-based residuals

#To find residuals based on MEDIANS, first calculate the medians for the 6 treatment classes
aggregate(clover_dat$NLevel, list(clover_dat$Culture), median)

#Create a medians column in the original dataset
clover_dat$medians<-c(rep(32.1,5),rep(24.8,5),rep(15.8,5),rep(20.5,5),rep(14.2,5),rep(19.1,5))

#Calculate residuals, based on medians
clover_dat$median_resids<-clover_dat$NLevel - clover_dat$medians

#Find the absolute values of those residuals
clover_dat$median_resids_abs<-abs(clover_dat$median_resids)
clover_dat

#Perform the ANOVA
leveneMEDIAN_ABS_mod<-lm(median_resids_abs ~ Culture, data = clover_dat)
anova(leveneMEDIAN_ABS_mod)
