#BIOL933
#Lab 3
#Example 3

#This script performs a one-way ANOVA, where subsamples have been averaged

#read in, re-classify, and inspect the data
mint_dat <- read.csv("Data/Lab3ex3.csv")

# mint_dat <- as.data.frame(mint_dat)
# mint_dat$Trtmt <- as.factor(mint_dat$Trtmt)
# mint_dat$Pot <- as.factor(mint_dat$Pot)
str(mint_dat, give.attr = F)

#The ANOVA
mint_mod<-lm(Growth ~ Trtmt, data = mint_dat)
anova(mint_mod)


#For the interested
#Instead of averaging Plants in Excel and re-importing, you could
#run the following code on the original, full dataset (Lab3ex2.csv)

#Calculating the Pot means
library(plyr)
mint_dat <- read.csv("Data/Lab3ex2.csv")
pot_means_dat <- ddply(mint_dat, c("Trtmt", "Pot"), summarise,
                   Growth = mean(Growth, na.rm=TRUE))

#The ANOVA
means_mod<-lm(Growth ~ Trtmt, data = pot_means_dat)
anova(means_mod)
