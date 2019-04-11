#BIOL933, Lab 5
#Example 3, replicated Latin Squares

#Read in, re-classify, and inspect the data
LS_dat <- read.csv("Data/Lab5ex3.csv")

#Inform R that Day, Tractor, and Driver are factors
LS_dat<-as.data.frame(LS_dat)
LS_dat$Day<-as.factor(LS_dat$Day)
LS_dat$Tractor<-as.factor(LS_dat$Tractor)
LS_dat$Driver<-as.factor(LS_dat$Driver)
LS_dat$Trtmt<-as.factor(LS_dat$Trtmt)
str(LS_dat, give.attr=F)

#Replicated LS sharing both rows (drivers) and columns (tractors)
LS1_mod<-lm(CO ~ Day + Tractor + Driver + Trtmt, LS_dat)
anova(LS1_mod)

#Replicated independent LS
#Here Tractor 1 on Day 1 is not the same as Tractor 1 on Day 2.
#Also, Driver 1 on Day 1 is different from Driver 1 on Day 2, etc.;
LS2_mod<-lm(CO ~ Day + Day:Tractor + Day:Driver + Trtmt, LS_dat)
anova(LS2_mod)

#Replicated LS sharing rows but with independent columns
#Here Driver 1 on Day 1 is the same as Driver 1 on Day 2, etc.;
#But for Tractors, we must specify the Day
LS3_mod<-lm(CO ~ Day + Day:Tractor + Driver + Trtmt, LS_dat)
anova(LS3_mod)

#Replicated LS sharing columns but with independent rows
#Here Tractor 1 on Day 1 is the same as Tractor 1 on Day 2, etc.;
#But for Drivers, we must specify the Day
LS4_mod<-lm(CO ~ Day + Tractor + Day:Driver + Trtmt, LS_dat)
anova(LS4_mod)
