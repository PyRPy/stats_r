#BIOL933
#Lab 1
#Script 2

#This script tests for normality and obtains descriptive statistics

#First, convert the imported data into a "dataframe" object
extract_dat<-as.data.frame(extract_dat)

#Generate a quantile-quantile (Q-Q, normal probability) plot
qqnorm(extract_dat$Extract, pch=19, col="red", main="Q-Q Plot for Extract Data")
qqline(extract_dat$Extract)

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(extract_dat$Extract)

#Get a look at the data
hist(extract_dat$Extract)

#Compute the min, max, mean, median, and 1st and 3rd quartiles; then visualize them
summary(extract_dat$Extract)
boxplot(extract_dat$Extract)

#Hypothesis Testing...conduct a one-sample t-test (two-tailed) for a mean
t.test(extract_dat$Extract, mu=78) #null H, mean = 78
power.t.test(n = length(extract_dat$Extract), delta = 78-mean(extract_dat$Extract), sd = sd(extract_dat$Extract), sig.level = 0.05, type = "one.sample", alternative = "two.sided", strict = TRUE)
