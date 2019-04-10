#BIOL933
#Lab 3
#Example 2

#This script performs a one-way ANOVA for a perfectly nested CRD and finds variance components

#read in, re-calssify, and inspect the data
mint_dat <- read.csv("Data/Lab3ex2.csv")

# mint_dat <- as.data.frame(mint_dat)
# str(mint_dat, give.attr = F)
# mint_dat$Trtmt <- as.factor(mint_dat$Trtmt)
# mint_dat$Pot <- as.factor(mint_dat$Pot)
# mint_dat$Plant <- as.factor(mint_dat$Plant)

str(mint_dat, give.attr = F)
head(mint_dat)
tail(mint_dat)

#Calculating components of variance
#install.packages("lme4")
library(lme4)
mintCV_mod<-lmer(Growth ~ Trtmt + (1|Trtmt:Pot), data = mint_dat)
summary(mintCV_mod)

#The ANOVA
mint_mod<-lm(Growth ~ Trtmt/Pot, data = mint_dat)
anova(mint_mod)

#The above model is short-hand for this:
mint2_mod<-lm(Growth ~ Trtmt + Trtmt:Pot, data = mint_dat)
anova(mint2_mod)

#Custom F-test for Trtmt
F_Trtmt<-35.928/2.153
F_Trtmt
pf(F_Trtmt, 5, 12, lower.tail = F)

# plot the data
library(ggplot2)
library(dplyr)

# interaction plot (kind of)
mint_dat %>% ggplot(aes(x=Trtmt, y=Growth))+
  geom_point()+
  geom_point(aes(group=Pot, color=Pot))

# box plot  
mint_dat %>% ggplot(aes(x=Trtmt, y=Growth))+
  geom_boxplot()
