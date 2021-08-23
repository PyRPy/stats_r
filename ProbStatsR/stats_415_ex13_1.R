
# One-Factor Analysis of Variance -----------------------------------------

dat <- read.csv("study.csv")
head(dat)
dat$Method <- as.factor(dat$Method)

linearReg <- lm(Score ~ Method, data = dat)

summary(linearReg)
aov(Score ~ Method, data = dat)
