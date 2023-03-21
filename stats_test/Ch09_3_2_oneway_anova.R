
# one way anova -----------------------------------------------------------
# Ho: mu1 = mu2 ... = mu5
# Ha: at least one of group mean different than others

# read data
dat <- read.delim("Ex9_3-2.txt")
head(dat)

# transform data
dat$Treat <- as.factor(dat$Treat)

# linear model
model_force <- lm(Observ ~ Treat, data = dat)
anova(model_force)

# Response: Observ
#           Df  Sum Sq Mean Sq F value    Pr(>F)
# Treat      4 16672.1  4168.0  44.202 3.664e-12 ***
# Residuals 30  2828.9    94.3

# visual observation
boxplot(Observ ~ Treat,
        data = dat,
        main = "Pull Forces in 5 Groups")

# conclusion : p-value < 0.05, reject Ho, there are differences among
# 5 groups of forces measured
