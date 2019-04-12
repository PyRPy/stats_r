#BIOL933, Lab 9
#Example 1

#This script performs a full ANOVA on a split-plot RCBD
oats_dat <- read.csv("Data/Lab9ex1.csv")
#Read in, re-classify, and inspect the data

oats_dat<-as.data.frame(oats_dat)
oats_dat$VarietyA<-as.factor(oats_dat$VarietyA)
oats_dat$Block<-as.factor(oats_dat$Block)
oats_dat$TrtmtB<-as.factor(oats_dat$TrtmtB)
str(oats_dat, give.attr=F)

#The ANOVA
oats_mod<-aov(Yield ~ VarietyA + Block + Error(VarietyA:Block) + TrtmtB + VarietyA:TrtmtB, oats_dat)
summary(oats_mod)

#Means comparisons
library(multcomp)

#Comparisons among subplot levels within a common main plot level
lot1_dat<-subset(oats_dat, VarietyA == 1)
lot1_mod<-lm(Yield ~ Block + TrtmtB, lot1_dat)
anova(lot1_mod)
lot1_dunnett=glht(lot1_mod,linfct=mcp(TrtmtB="Dunnett"))
confint(lot1_dunnett)

lot2_dat<-subset(oats_dat, VarietyA == 2)
lot2_mod<-lm(Yield ~ Block + TrtmtB, lot2_dat)
anova(lot2_mod)
lot2_dunnett=glht(lot2_mod,linfct=mcp(TrtmtB="Dunnett"))
confint(lot2_dunnett)

lot3_dat<-subset(oats_dat, VarietyA == 3)
lot3_mod<-lm(Yield ~ Block + TrtmtB, lot3_dat)
anova(lot3_mod)
lot3_dunnett=glht(lot3_mod,linfct=mcp(TrtmtB="Dunnett"))
confint(lot3_dunnett)

lot4_dat<-subset(oats_dat, VarietyA == 4)
lot4_mod<-lm(Yield ~ Block + TrtmtB, lot4_dat)
anova(lot4_mod)
lot4_dunnett=glht(lot4_mod,linfct=mcp(TrtmtB="Dunnett"))
confint(lot4_dunnett)

#Using loops in R to cycle through levels of the main plot A
A_levels<-c(1:4)
for (i in A_levels) {
  with(subset(oats_dat, VarietyA == A_levels[i]), {
       print(A_levels[i])
       print(anova(lm(Yield ~ Block + TrtmtB)))
       print(confint(glht(lm(Yield ~ Block + TrtmtB),linfct=mcp(TrtmtB="Dunnett"))))
  })
}

#Revisiting the same analysis for A by first averaging the subplots...
library(plyr)
oats_Bmeans_dat <- ddply(oats_dat, c("VarietyA", "Block"), summarise,
                       Yield = mean(Yield, na.rm=TRUE))
#...then carrying out the following ANOVA
oats_Bmeans_mod<-aov(Yield ~ VarietyA + Block, oats_Bmeans_dat)
summary(oats_Bmeans_mod)
