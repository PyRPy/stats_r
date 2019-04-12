#BIOL933
#Lab 10, example 1
#ANCOVA RCBD

#read in, re-classify, and inspect the data
lima_dat <- read.csv("Lab10ex1.csv")

lima_dat<-as.data.frame(lima_dat)
lima_dat$Block<-as.factor(lima_dat$Block)
lima_dat$Variety<-as.factor(lima_dat$Variety)
str(lima_dat, give.attr=F)

# plot the data regarding variety
library(ggplot2)
ggplot(lima_dat, aes(x=X, y=Y, color=Variety))+
  geom_point()+
  facet_wrap(~Variety)

ggplot(lima_dat, aes(x=Variety, y=Y))+
  geom_boxplot()

# 1. General regression
plot(Y ~ X, lima_dat)
reg_mod<-lm(Y ~ X, lima_dat)
anova(reg_mod)
summary(reg_mod)

# 2.  Perform ANOVA with X as Response
anovaX_mod<-lm(X ~ Block + Variety, lima_dat)
anova(anovaX_mod)

# 3.  Perform ANOVA with Y as Response
anovaY_mod<-lm(Y ~ Block + Variety, lima_dat)
anova(anovaY_mod)
summary(anovaY_mod)

# 4.  Test for homogeneity of slopes
slopes_mod<-lm(Y ~ Block + Variety + X + Variety*X, lima_dat)
anova(slopes_mod)

# 5.  The ANCOVA
library(car)
ancova_mod<-lm(Y ~ Block + Variety + X, lima_dat)
Anova(ancova_mod, type = 2)

# 6.  Find beta and Xmean; then create Z
summary(ancova_mod)
mean(lima_dat$X)
lima_dat$Z<-lima_dat$Y + 3.1320*(lima_dat$X - 33.98727)

# INTERLUDE:  Comparing means and analyses
library(lsmeans)
lima_lsm <- lsmeans(ancova_mod, "Variety")
adj_means <- aggregate(lima_dat$Z, list(lima_dat$Variety), mean)

anovaZ_mod<-lm(Z ~ Block + Variety, lima_dat)
anova(anovaZ_mod)

# 7.  Normality of residuals
anovaZ_mod<-lm(Z ~ Block + Variety, lima_dat)
lima_dat$anovaZ_resids <- residuals(anovaZ_mod)
shapiro.test(lima_dat$anovaZ_resids)

# 8.  Homogeneity of variances
library(car)
leveneTest(Z ~ Variety, data = lima_dat)

# 9.  Additivity of main effects
lima_dat$anovaZ_preds <- predict(anovaZ_mod)
lima_dat$sq_anovaZ_preds <- lima_dat$anovaZ_preds^2
tukeyZ_mod<-lm(Z ~ Block + Variety + sq_anovaZ_preds, lima_dat)
anova(tukeyZ_mod)

# 10.  The ANCOVA, with desired subsequent analysis
ancova_mod<-lm(Y ~ Block + Variety + X, lima_dat)
Anova(ancova_mod, type = 2)

#Tukey separation of adjusted variety means
lima_lsm <- lsmeans(ancova_mod, "Variety")
contrast(lima_lsm, method = "pairwise", adjust = "tukey")
