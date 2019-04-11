#BIOL933
#Lab 4, example 3

#This script illustrates the use of means separations tests

#read in, inspect, and re-classify the data
clover_dat <- read.csv("Data/Lab4ex3.csv") # string as factor at default

# clover_dat<-as.data.frame(clover_dat)
# clover_dat$Culture<-as.factor(clover_dat$Culture)
str(clover_dat, give.attr=F)

clover_mod<-lm(NLevel ~ Culture, data = clover_dat)
anova(clover_mod)

# Install the required package 
# LSD and other posthoc tests are not in the default packages; 
# the package “agricolae” contains scripts for LSD, Scheffe, Duncan, and SNK tests, 
# among others. Agricolae was developed by Felipe de Mendiburu as part of his master thesis 
# "A statistical analysis tool for agricultural research" – Univ. Nacional de Ingenieria, 
# Lima-Peru (UNI).

# install.packages("agricolae") - somehow 'agricolae' not working !
library(agricolae)

#FIXED RANGE TESTS
#LSD
LSD <- LSD.test(clover_mod, "Culture")

#Tukey HSD
Tukey <- HSD.test(clover_mod, "Culture")

#Scheffe
Scheffe <- scheffe.test(clover_mod, "Culture")

#MULTIPLE RANGE TESTS
#Duncan
Duncan <- duncan.test(clover_mod, "Culture")

#SNK
SNK <- SNK.test(clover_mod, "Culture")


#---Dunnett Test---
#For this, install package "multcomp"; some less convenient syntax is needed

#install.packages("multcomp")
#library(multcomp)

#The Dunnett Test uses the first treatment (alphanumerically) as the "control" -- hence renamed 1Comp
#Also note in the output that the "Quantile" (2.6957) is the Critical Value of Dunnett's t

test.dunnett=glht(clover_mod,linfct=mcp(Culture="Dunnett"))
confint(test.dunnett)


#---REGWQ Test---
#For this, package "mutoss" is needed.  Unfortunately, this package has a dependency
#called "multtest" which is no longer available on CRAN.  "multtest" is
#available, however, through bioconductor.  To install, run the following lines:

source("http://bioconductor.org/biocLite.R")
biocLite("multtest")

#Then install the package "mutoss" as usual.  The REGWQ analysis:
#library(mutoss)
REGWQ<-regwq(clover_mod, clover_dat, alpha = 0.05)


#As always, in thinking about the results of various analyses, it is useful to visualize the data
plot(clover_dat, main = "Boxplot comparing treatment means")

# somehow 'agricolae' not working
model.aov <- aov(NLevel ~ Culture, data=clover_dat)
TukeyHSD(model.aov)
