# BIOL933, Lab 7
# Example 1

# This script performs an ANOVA on an 3x5 factorial RBCD, where the interaction 
# is significant

#read in, re-classify, and inspect the data
must_dat <- read.csv("Lab7ex1.csv")

must_dat<-as.data.frame(must_dat)
must_dat$Soil<-as.factor(must_dat$Soil)
must_dat$Var<-as.factor(must_dat$Var)
must_dat$Block<-as.factor(must_dat$Block)
str(must_dat, give.attr=F)

# plot the data first ??? 
# first step !!!
library(ggplot2)
ggplot(must_dat, aes(x=Soil, y=Yield, color=Var))+
  geom_point()

#The ANOVA
must_mod<-lm(Yield ~ Block + Soil*Var, must_dat)
anova(must_mod)


#Exploratory model to look at 2-way Block interactions
must.explor_mod<-lm(Yield ~ (Block + Soil + Var)^2, must_dat)
anova(must.explor_mod)


#Analyze the simple effects of Var by subsetting the data...
must_Soil1_dat<-subset(must_dat, must_dat$Soil == "1")
must_Soil2_dat<-subset(must_dat, must_dat$Soil == "2")
must_Soil3_dat<-subset(must_dat, must_dat$Soil == "3")

#...and then performing multiple ANOVAs
anova(lm(Yield ~ Block + Var, must_Soil1_dat))
anova(lm(Yield ~ Block + Var, must_Soil2_dat))
anova(lm(Yield ~ Block + Var, must_Soil3_dat))

#Tukey mean separations of Var, within each level of Soil
#install.packages("agricolae")
#library(agricolae)
# tukey_s1 <- HSD.test(lm(Yield ~ Block + Var, must_Soil1_dat), "Var")
# tukey_s2 <- HSD.test(lm(Yield ~ Block + Var, must_Soil2_dat), "Var")
# tukey_s3 <- HSD.test(lm(Yield ~ Block + Var, must_Soil3_dat), "Var")


#Analyze the simple effects of Soil by subsetting the data...
must_Var1_dat<-subset(must_dat, must_dat$Var == "1")
must_Var2_dat<-subset(must_dat, must_dat$Var == "2")
must_Var3_dat<-subset(must_dat, must_dat$Var == "3")
must_Var4_dat<-subset(must_dat, must_dat$Var == "4")
must_Var5_dat<-subset(must_dat, must_dat$Var == "5")

#...and then performing multiple ANOVAs
anova(lm(Yield ~ Block + Soil, must_Var1_dat))
anova(lm(Yield ~ Block + Soil, must_Var2_dat))
anova(lm(Yield ~ Block + Soil, must_Var3_dat))
anova(lm(Yield ~ Block + Soil, must_Var4_dat))
anova(lm(Yield ~ Block + Soil, must_Var5_dat))

# Tukey mean separations of Soil, within each level of Var
# tukey_v1 <- HSD.test(lm(Yield ~ Block + Soil, must_Var1_dat), "Soil")
# tukey_v2 <- HSD.test(lm(Yield ~ Block + Soil, must_Var2_dat), "Soil")
# tukey_v3 <- HSD.test(lm(Yield ~ Block + Soil, must_Var3_dat), "Soil")
# tukey_v4 <- HSD.test(lm(Yield ~ Block + Soil, must_Var4_dat), "Soil")
# tukey_v5 <- HSD.test(lm(Yield ~ Block + Soil, must_Var5_dat), "Soil")


#Generate interaction plots
#install.packages("HH")
library(HH)
intxplot(Yield ~ Var, groups = Soil, data=must_dat, se=TRUE, ylim=range(must_dat$Yield), offset.scale=500)
intxplot(Yield ~ Soil, groups = Var, data=must_dat, se=TRUE, ylim=range(must_dat$Yield), offset.scale=500)


#TEMPLATE For the interested: making a barplot of main effects using tapply

#first, get the means, by Soil type
mean.Yield <- tapply(must_dat$Yield, list(must_dat$Soil), mean)
#then, get the standard deviations, by Soil type
sd.Yield <- tapply(must_dat$Yield, list(must_dat$Soil), sd)
#finally, get the sample sizes, again by Soil type
n.Yield <- tapply(must_dat$Yield, list(must_dat$Soil), length)
se.Yield <- sd.Yield/(n.Yield)**(1/2)

barplot(mean.Yield) #makes a simple barplot!

#Pimp your barplot
mids <- barplot(mean.Yield,
	beside = TRUE, legend = TRUE,
	xlab = "Soil",
	ylab = "Yield",
	ylim = c(0,25),
	col=grey(c(0.4,0.7,1)))

#now, to add error bars, we assign the barplot above to an object called "mids"
arrows(mids, mean.Yield - 2*se.Yield, mids, mean.Yield + 2*se.Yield, code = 3, angle = 90, length = 0.1)

#now add text, labeling the bars
text(mids, 2, paste(n.Yield), col=c("white", rep("black", 3)))
