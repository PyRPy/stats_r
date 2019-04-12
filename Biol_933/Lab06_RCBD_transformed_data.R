#BIOL933, Lab 6
#Example 1

#read in, re-classify, and inspect the data
vit_dat <- read.csv("Lab6ex1.csv")

vit_dat<-as_data.frame(vit_dat)
vit_dat$block<-as.factor(vit_dat$block)
vit_dat$trtmt<-as.factor(vit_dat$trtmt)
str(vit_dat, give.attr = F)

library(ggplot2)
ggplot(vit_dat, aes(x=trtmt, y=weight, color=block))+
  geom_point()

#The ANOVA
vit_mod<-lm(weight ~ trtmt + block, vit_dat)
anova(vit_mod)

#Need to assign contrast coefficients
#Notice from str() that R orders the Trtmt levels this way: chick_cont, 
# chick_vit, mice_cont, ...
# Our desired contrasts:
# Contrast ‘Mam vs. Bird’         2,2,-1,-1,-1,-1
# Contrast ‘Mouse vs. Sheep'      0,0,1,1,-1,-1
# Contrast ‘Vit’                  1,-1,1,-1,1,-1
# Contrast ‘MamBird*Vit’	        2,-2,-1,1,-1,1
# Contrast ‘MouShe*Vit’           0,0,1,-1,-1,1

contrastmatrix<-cbind(c(2,2,-1,-1,-1,-1),c(0,0,1,1,-1,-1),c(1,-1,1,-1,1,-1),
                      c(2,-2,-1,1,-1,1),c(0,0,1,-1,-1,1))
contrasts(vit_dat$trtmt)<-contrastmatrix

log_contrast_mod<-aov(weight ~ trtmt + block, vit_dat)
summary(log_contrast_mod, 
        split = list(trtmt = list("MvsB" = 1, "MvsS" = 2, "Vit" = 3, "MB*Vit" = 4, "MS*Vit" = 5))) 

#TESTING ASSUMPTIONS
#Generate residual and predicted values
vit_dat$resids <- residuals(vit_mod)
vit_dat$preds <- predict(vit_mod)
vit_dat$sq_preds <- vit_dat$preds^2

#Look at a plot of residual vs. predicted values
plot(resids ~ preds, data = vit_dat,
	xlab = "Predicted Values",
	ylab = "Residuals")

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(vit_dat$resids)

#Perform Levene's Test for homogenity of variances
#install.packages("car")
library(car)
leveneTest(weight ~ trtmt, data = vit_dat)

#Perform a Tukey 1-df Test for Non-additivity
log_1df_mod<-lm(weight ~ trtmt + block + sq_preds, vit_dat)
anova(log_1df_mod)

# -----PART II-----
  
#Create a log-transformed variable
vit_dat$trans_weight<-log10(10*vit_dat$weight)

#The ANOVA
trans_vit_mod<-lm(trans_weight ~ trtmt + block, vit_dat)
anova(trans_vit_mod)

trans_log_contrast_mod<-aov(trans_weight ~ trtmt + block, vit_dat)
summary(trans_log_contrast_mod, split = list(trtmt = list("MvsB" = 1, "MvsS" = 2, "Vit" = 3, "MB*Vit" = 4, "MS*Vit" = 5))) 

#TESTING ASSUMPTIONS
#Generate residual and predicted values
vit_dat$trans_resids <- residuals(trans_vit_mod)
vit_dat$trans_preds <- predict(trans_vit_mod)
vit_dat$sq_trans_preds <- vit_dat$trans_preds^2

#Look at a plot of residual vs. predicted values
plot(trans_resids ~ trans_preds, data = vit_dat,
     xlab = "Predicted Values",
     ylab = "Residuals")

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(vit_dat$trans_resids)

#Perform Levene's Test for homogenity of variances
leveneTest(trans_weight ~ trtmt, data = vit_dat)

#Perform a Tukey 1-df Test for Non-additivity
trans_log_1df_mod<-lm(trans_weight ~ trtmt + block + sq_trans_preds, vit_dat)
anova(trans_log_1df_mod)

# check the plot again
ggplot(vit_dat, aes(x=trtmt, y=trans_weight, color=block))+
  geom_point()
