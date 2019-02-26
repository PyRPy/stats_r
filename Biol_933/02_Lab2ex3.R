#BIOL933
#Lab 2
#Example 3
extract_dat <- read.csv('Lab2ex3.csv', header = TRUE)
#This script tests for normality, performs a t-test, and calculates power

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(extract_dat$Extract)

#Hypothesis Testing...conduct a one-sample t-test (two-tailed) for a mean
t.test(extract_dat$Extract, mu=78) #null H, mean = 78
power.t.test(n = length(extract_dat$Extract),
             delta = 78-mean(extract_dat$Extract),
             sd = sd(extract_dat$Extract),
             sig.level = 0.05,
             type = "one.sample",
             alternative = "two.sided")


# Other variations of the t-test

# 1. Independent samples without assumption of equal variances
height <- c(2,2,2,4)
height2 <- c(2,2,3,5)
t.test(height, height2)

# 2. Independent samples, assuming equal variances
t.test(height, height2, var.equal = T)
# To estimate the power of this test, you need to find the pooled variance
pooled.var <- (1/2)*(var(height)+var(height2))
# Then you can find the power
power.t.test(n = length(height),
             delta = mean(height)-mean(height2),
             sd = sqrt(pooled.var),
             sig.level = 0.05,
             type = "two.sample",
             alternative = "two.sided")

# 3. Paired (not independent) samples
t.test(height, height2, paired=T)
# To estimate the power of this test, you need to creat a new variable of the differences
diff<-height-height2
# Then you can find the power
power.t.test(n = length(height),
             delta = mean(height)-mean(height2),
             sd = sd(diff),
             sig.level = 0.05,
             type = "paired",
             alternative = "two.sided")
