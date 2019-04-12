#BIOL933, Lab 9
#Example 2

#This script performs a full ANOVA on a split-plot CRD

#Read in, re-classify, and inspect the data
oats_dat <- read.csv("Data/Lab9ex2.csv")

oats_dat<-as.data.frame(oats_dat)
oats_dat$VarietyA<-as.factor(oats_dat$VarietyA)
oats_dat$Rep<-as.factor(oats_dat$Rep)
oats_dat$TrtmtB<-as.factor(oats_dat$TrtmtB)
str(oats_dat, give.attr=F)

#The ANOVA --- not 'blocking' as a factor showing up in the formula --- !
oats_mod<-aov(Yield ~ VarietyA + Error(VarietyA:Rep) + TrtmtB + VarietyA:TrtmtB, oats_dat)
summary(oats_mod)

#Means comparisons
library(multcomp)

#Comparisons among subplot levels within a common main plot level
lot1_dat<-subset(oats_dat, VarietyA == 1)
lot1_mod<-lm(Yield ~ TrtmtB, lot1_dat)
anova(lot1_mod)
lot1_dunnett=glht(lot1_mod,linfct=mcp(TrtmtB="Dunnett"))
confint(lot1_dunnett)

#Etc....or...

#Using loops in R to cycle through levels of the main plot A
A_levels<-c(1:4)
for (i in A_levels) {
  with(subset(oats_dat, VarietyA == A_levels[i]), {
       print(A_levels[i])
       print(anova(lm(Yield ~ TrtmtB)))
       print(confint(glht(lm(Yield ~ TrtmtB),linfct=mcp(TrtmtB="Dunnett"))))
  })
}
