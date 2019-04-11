#BIOL933
#Soybean spacing example, Lab 4 example 2

#This script performs a trend analysis using orthogonal contrasts (CRD), then using regression

#read in, inspect, and re-classify the data
spacing_dat <-read.csv("Data/Lab4ex2.csv")

spacing_dat<-as.data.frame(spacing_dat)
spacing_dat$Spacing<-as.factor(spacing_dat$Spacing)
str(spacing_dat, give.attr=F)

#The ANOVA
spacing_mod<-lm(Yield ~ Spacing, spacing_dat)
anova(spacing_mod)

#Need to assign contrast coefficients
# Contrast ‘Linear’		-2,-1,0,1,2
# Contrast ‘Quadratic’	2,-1,-2,-1,2
# Contrast ‘Cubic’	-1,2,0,-2,1
# Contrast ‘Quartic’	1,-4,6,-4,1
contrastmatrix<-cbind(c(-2,-1,0,1,2),c(2,-1,-2,-1,2),c(-1,2,0,-2,1),c(1,-4,6,-4,1))
contrastmatrix

contrasts(spacing_dat$Spacing)<-contrastmatrix
spacing_dat$Spacing

spacing_contrast_mod<-aov(Yield ~ Spacing, spacing_dat)
summary(spacing_contrast_mod, split = list(Spacing = list("Linear" = 1, "Quadratic" = 2, "Cubic" = 3, "Quartic" = 4))) 

spacing_con_mod<-lm(Yield ~ Spacing, spacing_dat)
summary(spacing_con_mod)


#We can carry out the same analysis using a multiple regression approach
#To do this, you must read in the dataset again
spacing_dat<-as.data.frame(spacing_dat)

#For regression, Spacing is no longer a classification variable, it is a numeric regression variable:
spacing_dat$Spacing<-as.numeric(spacing_dat$Spacing)
str(spacing_dat)

Spacing<-spacing_dat$Spacing
Spacing2<-Spacing^2
Spacing3<-Spacing^3
Spacing4<-Spacing^4

anova(lm(Yield ~ Spacing + Spacing2 + Spacing3 + Spacing4, spacing_dat))


#For the interested: best-fit line
#To calculate a best-fit line, ask R to regress yield on a polynomial (degree 4) of Spacing
lm(Yield ~ poly(Spacing,2,raw=TRUE), spacing_dat)
