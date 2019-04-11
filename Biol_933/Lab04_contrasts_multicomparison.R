#BIOL933 I
#Lab 4, example 1

#This script performs a class comparison analysis using orthogonal contrasts (CRD)

#read in, inspect, and re-classify the data
cleome_dat <- read.csv("Data/Lab4ex1.csv")
# cleome_dat<-as_data.frame(cleome_dat)
# cleome_dat$Trtmt<-as.factor(cleome_dat$Trtmt)
str(cleome_dat, give.attr=F)

# plot the data
library(ggplot2)
ggplot(cleome_dat, aes(x=Trtmt, y=Growth))+
  geom_boxplot()

#The ANOVA
cleome_mod<-lm(Growth ~ Trtmt, cleome_dat)
anova(cleome_mod)

#Need to assign contrast coefficients
#Notice from str() that R orders the Trtmt levels this way: H08, H12, H16, L08, L12, L16
# Our desired contrasts:
# Contrast ‘Temp’					1,1,1,-1,-1,-1
# Contrast ‘Light Linear'			1,0,-1,1,0,-1
# Contrast ‘Light Quadratic’		1,-2,1,1,-2,1
# Contrast ‘Temp * Light Linear’	1,0,-1,-1,0,1
# Contrast ‘Temp * Light Quadratic’	1,-2,1,-1,2,-1

contrastmatrix<-cbind(c(1,1,1,-1,-1,-1),c(1,0,-1,1,0,-1),
                      c(1,-2,1,1,-2,1),c(1,0,-1,-1,0,1),c(1,-2,1,-1,2,-1))
contrastmatrix

contrasts(cleome_dat$Trtmt)<-contrastmatrix
cleome_dat$Trtmt

cleome_contrast_mod<-aov(Growth ~ Trtmt, cleome_dat)
summary(cleome_contrast_mod, 
        split = list(Trtmt = list("Temp" = 1, "Light Lin" = 2, "Light Quad" = 3, "T*L Lin" = 4, "T*L Quad" = 5))) 


#In general, people do not report contrast SS; so in practice you can simply use lm(), as usual:
cleome_cont_mod<-lm(Growth ~ Trtmt, cleome_dat)
summary(cleome_cont_mod)
