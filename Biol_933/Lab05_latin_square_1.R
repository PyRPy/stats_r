#BIOL933, Lab 5
#Example 2, unreplicated Latin Square

#Read in, re-classify, and inspect the data
LS_dat <- read.csv("Data/Lab5ex2.csv")

#Inform R that Row and Col are factors
LS_dat<-as.data.frame(LS_dat)
LS_dat$Row<-as.factor(LS_dat$Row)
LS_dat$Col<-as.factor(LS_dat$Col)
LS_dat$Trtmt<-as.factor(LS_dat$Trtmt)
str(LS_dat, give.attr=F)

#The ANOVA
LS_mod<-lm(Response ~ Row + Col + Trtmt, LS_dat)
anova(LS_mod)

#TESTING ASSUMPTIONS
#Generate residual and predicted values
LS_dat$resids <- residuals(LS_mod)
LS_dat$preds <- predict(LS_mod)

#Look at a plot of residual vs. predicted values
plot(resids ~ preds, data = LS_dat,
     xlab = "Predicted Values",
     ylab = "Residuals")

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(LS_dat$resids)

#Perform Levene's Test for homogenity of variances among treatment levels
library(car)
leveneTest(Response ~ Trtmt, data = LS_dat)

#Testing for significance of the Row*Col interaction
LS_RC_mod<-lm(Response ~ Row + Col, LS_dat)
RC_preds <- predict(LS_RC_mod)
LS_dat$RC_sqpreds <- RC_preds^2

LS_RC_Tukey_mod<-lm(Response ~ Row + Col + RC_sqpreds, LS_dat)
anova(LS_RC_Tukey_mod)

#Testing for significance of the Row*Trtmt interaction
LS_RT_mod<-lm(Response ~ Row + Trtmt, LS_dat)
RT_preds <- predict(LS_RT_mod)
LS_dat$RT_sqpreds <- RT_preds^2

LS_RT_Tukey_mod<-lm(Response ~ Row + Trtmt + RT_sqpreds, LS_dat)
anova(LS_RT_Tukey_mod)

#Testing for significance of the Col*Trtmt interaction
LS_CT_mod<-lm(Response ~ Col + Trtmt, LS_dat)
CT_preds <- predict(LS_CT_mod)
LS_dat$CT_sqpreds <- CT_preds^2

LS_CT_Tukey_mod<-lm(Response ~ Col + Trtmt + CT_sqpreds, LS_dat)
anova(LS_CT_Tukey_mod)

#Since all assumptions are met, feel free to continue the analysis (contrasts, separations, etc.)
#using the original model.  For example:

#Tukey HSD
Tukey <- HSD.test(LS_mod, "Trtmt") # not working

#Or, if you were using contrasts INSTEAD OF all pairwise comparisons:
#Contrast  'AB vs. CD'  (1, 1, -1, -1)
#Contrast  'A vs. B'  (1, -1, 0, 0)
#Contrast  'C vs. D'  (0, 0, 1, -1)

contrastmatrix<-cbind(c(1,1,1,-3), c(1,-1,0,0), c (0,0,1,-1))
contrasts(LS_dat$Trtmt)<-contrastmatrix

LS_contrast_mod<-aov(Response ~ Row + Col + Trtmt, LS_dat)
summary(LS_contrast_mod, split = list(Trtmt = list("AB vs CD" = 1, "A vs B" = 2, "C vs D" = 3))) 

LS_cont_mod<-lm(Response ~ Row + Col + Trtmt, LS_dat)
summary(LS_cont_mod)
