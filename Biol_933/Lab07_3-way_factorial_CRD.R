#BIOL933, Lab 7
#Example 2

#This script performs an ANOVA on an 3-way factorial CRD

#read in, re-classify, and inspect the data
fact_dat <- read.csv("Lab7ex2.csv")

fact_dat<-as.data.frame(fact_dat)
fact_dat$A<-as.factor(fact_dat$A)
fact_dat$B<-as.factor(fact_dat$B)
fact_dat$C<-as.factor(fact_dat$C)
str(fact_dat, give.attr=F)


#The ANOVA (v1)
fact_mod1<-lm(Y ~ A*B*C, fact_dat)
anova(fact_mod1)

#The ANOVA (v2)
fact_mod2<-lm(Y ~ (A + B + C)^2, fact_dat)
anova(fact_mod2)


#Create the c1-c2 variable and re-import the data
# read the data again
fact_dat <- read.csv("Lab7ex2b.csv")

fact_dat<-as.data.frame(fact_dat)
fact_dat$A<-as.factor(fact_dat$A)
fact_dat$B<-as.factor(fact_dat$B)
str(fact_dat, give.attr=F)

#Generate 3-way interaction plot
#install.packages("HH")
library(HH)
intxplot(c1_c2 ~ A, groups = B, data=fact_dat, se=TRUE, ylim=range(fact_dat$c1_c2), offset.scale=500)
intxplot(c1_c2 ~ B, groups = A, data=fact_dat, se=TRUE, ylim=range(fact_dat$c1_c2), offset.scale=500)
