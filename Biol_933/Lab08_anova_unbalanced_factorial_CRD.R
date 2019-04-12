#BIOL933, Lab 8
#Example 2
# --- For unbalanced designs, use LS Means and partial SS.--- #

#This script performs an ANOVA on an unbalanced factorial CRD

#read in, re-classify, and inspect the data
miss_dat <- read.csv("Data/Lab8ex2.csv")
miss_dat<-as.data.frame(miss_dat)
miss_dat$A<-as.factor(miss_dat$A)
miss_dat$B<-as.factor(miss_dat$B)
str(miss_dat, give.attr=F)

# plot the data first
library(ggplot2)
ggplot(miss_dat, aes(x=B, y=Y, color=A))+
  geom_point()

#The ANOVA
miss1_mod<-lm(Y ~ A + B + A:B, miss_dat)
anova(miss1_mod)

miss2_mod<-lm(Y ~ B + A + A:B, miss_dat)
anova(miss2_mod)

#The ANOVA, with partial (or Type II) SS
library(car)
Anova(miss1_mod, type=2)
Anova(miss2_mod, type=2)

#Finding the unadjusted means
meansA <- aggregate(miss_dat$Y, list(miss_dat$A), mean)
meansB <- aggregate(miss_dat$Y, list(miss_dat$B), mean)
meansA
meansB

#Computing LSMeans
library(lsmeans)
lsmeans(miss1_mod, "A")
lsmeans(miss1_mod, "B")

#Comparing LSMeans, using the "lsmeans" package (function contrast())
missA.lsm <- lsmeans(miss1_mod, "A")
missB.lsm <- lsmeans(miss1_mod, "B")

#Tukey comparisons
contrast(missA.lsm, method = "pairwise", adjust = "tukey")
contrast(missB.lsm, method = "pairwise", adjust = "tukey")

#Dunnett test
contrast(missB.lsm, method = "trt.vs.ctrl")

#Contrasts
contrast(missB.lsm, list("1 vs. 23"=c(2,-1,-1), "2 vs. 3"=c(0,1,-1)))

#Trend analysis
contrast(missB.lsm, method = "poly")
