
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
t.test(write, mu = 50, alternative = "greater")

qqnorm(write)
qqline(write, col = "blue", lwd = 2)


# One sample median test --------------------------------------------------
boxplot(write) # median is close to 55
wilcox.test(write, mu = 50)
median(write) # 54


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


# Kruskal Wallis test -----------------------------------------------------

kruskal.test(write, prog)

# Paired t-test -----------------------------------------------------------

t.test(write, read, paired = TRUE)
boxplot(write, read) # overlap

# Wilcoxon signed rank sum test -------------------------------------------

wilcox.test(write, read, paired = TRUE)

boxplot(Ozone ~ Month, data = airquality)

wilcox.test(Ozone ~ Month, data = airquality,
        subset = Month %in% c(5, 8))

# McNemar test ------------------------------------------------------------
X <- matrix(c(172, 7, 6, 15), 2, 2)
mcnemar.test(X)

## Agresti (1990), p. 350.
## Presidential Approval Ratings.
## Approval of the President's performance in office in two surveys,
## one month apart, for a random sample of 1600 voting-age Americans.
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                         "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)

# One-way repeated measures ANOVA -----------------------------------------
library(MASS)
library(car)
require(foreign)
kirk <- within(read.dta("https://stats.idre.ucla.edu/stat/stata/examples/kirk/rb4.dta"), 
               {
                 s <- as.factor(s)
                 a <- as.factor(a)
               })

model <- lm(y ~ a + s, data = kirk)
analysis <- Anova(model, idata = kirk, idesign = ~s)
print(analysis)

# Repeated measures logistic regression -----------------------------------

require(lme4)
exercise <- within(read.dta("https://stats.idre.ucla.edu/stat/stata/whatstat/exercise.dta"), 
                   {
                     id <- as.factor(id)
                     diet <- as.factor(diet)
                   })
glmer(highpulse ~ diet + (1 | id), data = exercise, family = binomial)

# Factorial ANOVA ---------------------------------------------------------
anova(lm(write ~ female * ses, data = hsb2))


# Friedman test -----------------------------------------------------------
friedman.test(cbind(read, write, math))

# Factorial logistic regression -------------------------------------------

summary(glm(female ~ prog * schtyp, data = hsb2, family = binomial))

# Correlation -------------------------------------------------------------
cor(read, write)
plot(read, write)

cor.test(read, write)

# Simple linear regression ------------------------------------------------
lm(write ~ read)
summary(lm(write ~ 0 + read))

# Non-parametric correlation ----------------------------------------------

cor.test(write, read, method = "spearman")

# Simple logistic regression ----------------------------------------------

glm(female ~ read, family = binomial)

# Multiple regression -----------------------------------------------------
lm(write ~ female + read + math + science + socst)

# Analysis of covariance --------------------------------------------------

summary(aov(write ~ prog + read))

# Multiple logistic regression --------------------------------------------

glm(female ~ read + write, family = binomial)

# Ordered logistic regression ---------------------------------------------
require(MASS)

## Create order variable write3 as a factor with levels 1, 2, and 3
hsb2$write3 <- cut(hsb2$write, c(0, 48, 57, 70),  right = TRUE, labels = c(1,2,3))
table(hsb2$write3)

## fit ordered logit model and store results 'm'
m <- polr(write3 ~ female + read + socst, data = hsb2, Hess=TRUE)

## view a summary of the model
summary(m)

# Discriminant analysis ---------------------------------------------------
fit <- lda(factor(prog) ~ read + write + math, data = hsb2)
fit # show results 

# One-way MANOVA ----------------------------------------------------------

summary(manova(cbind(read, write, math) ~ prog))

# Multivariate multiple regression ----------------------------------------

M1 <- lm(cbind(write, read) ~ female + math + science + socst, data = hsb2)

require(car)
summary(Anova(M1))

# Canonical correlation ---------------------------------------------------

require(CCA)
cc(cbind(read, write), cbind(math, science))

# Factor analysis ---------------------------------------------------------

library(psych)
fa(r = cor(model.matrix(~read + write + math + science + socst - 1, data = hsb2)), 
   rotate = "none", fm = "pa", 2)

# Principal components analysis -------------------------------------------
princomp(formula = ~read + write + math + science + socst, data = hsb2)

# write.csv(exercise, "excercise.csv")
# write.csv(kirk, "kirk.csv")
