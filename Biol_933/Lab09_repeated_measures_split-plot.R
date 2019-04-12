#BIOL933
#Lab 9, Example 3

#This script performs a repeated measures analysis of a CRD

#Read in, re-classify, and inspect the data
hist_dat <- read.csv("Data/Lab9ex3.csv")

hist_dat<-as.data.frame(hist_dat)
hist_dat$Drug<-as.factor(hist_dat$Drug)
hist_dat$Hist<-as.factor(hist_dat$Hist)
hist_dat$Time<-as.factor(hist_dat$Time)
hist_dat$Dog<-as.factor(hist_dat$Dog)
str(hist_dat, give.attr=F)

#Repeated measures analysis
library(ez)

ezANOVA(
  data = hist_dat,
  dv = H,
  wid = Dog,
  within = Time,
  between = Drug:Hist,
  return_aov = TRUE
)


#APPENDIX: Full analysis
#The split-plot ANOVA
hist_mod<-aov(H ~ Drug + Hist + Drug:Hist + Error(Drug:Hist:Dog) + Drug*Hist*Time, hist_dat)
summary(hist_mod)

# plot the trend
library(ggplot2)
par(mfrow=c(1,2))
ggplot(hist_dat, aes(x=Time, y=H, color=Drug))+
  geom_point()

ggplot(hist_dat, aes(x=Time, y=H, color=Hist))+
  geom_point()

ggplot(hist_dat, aes(x=Time, y=H))+
  geom_boxplot()+
  facet_wrap(~Drug)

ggplot(hist_dat, aes(x=Time, y=H))+
  geom_boxplot()+
  facet_wrap(~Hist)
