
# Lesson 28: Choosing Appropriate Statistical Methods ---------------------


# WHAT STATISTICAL ANALYSIS SHOULD I USE? ---------------------------------
# https://stats.idre.ucla.edu/r/whatstat/what-statistical-analysis-should-i-usestatistical-analyses-using-r/


# Introduction ------------------------------------------------------------
# various stats method, what method is suitable for your applications


# Setup and Data ----------------------------------------------------------
hsb2 <- within(read.csv("https://stats.idre.ucla.edu/stat/data/hsb2.csv"), {
  race <- as.factor(race)
  schtyp <- as.factor(schtyp)
  prog <- as.factor(prog)
})
# write.csv(hsb2, "hsb2.csv")
attach(hsb2)
head(hsb2)

#    id female race ses schtyp prog read write math science socst
# 1  70      0    4   1      1    1   57    52   41      47    57
# 2 121      1    4   2      1    3   68    59   53      63    61
# 3  86      0    4   3      1    1   44    33   54      58    31
# 4 141      0    4   3      1    3   63    44   47      53    56
# 5 172      0    4   2      1    2   47    52   57      53    61
# 6 113      0    4   2      1    2   44    52   51      63    61

# One sample t-test -------------------------------------------------------

t.test(write, mu = 50)
hist(write)

qqnorm(write)
qqline(write, col = "blue", lwd = 2)


# One sample median test --------------------------------------------------
boxplot(write) # median is close to 55
wilcox.test(write, mu = 50)
median(write)


# Binomial test -----------------------------------------------------------
prop.test(sum(female), length(female), p = 0.5)

phat = sum(female) / length(female) # 0.545


# Chi-square goodness of fit ----------------------------------------------

chisq.test(table(race), p = c(10, 10, 10, 70) / 100)
table(race)


# Two independent samples t-test ------------------------------------------

t.test(write ~ female)
boxplot(write ~ female)


# Wilcoxon-Mann-Whitney test ----------------------------------------------

wilcox.test(write ~ female)


# Chi-square test ---------------------------------------------------------
table(female, schtyp)
chisq.test(table(female, schtyp))


# Fisher’s exact test -----------------------------------------------------
table(race, schtyp)
fisher.test(table(race, schtyp))


# One-way ANOVA -----------------------------------------------------------
summary(aov(write ~ prog))
plot(prog, write, xlab = "prog", ylab = "write")
