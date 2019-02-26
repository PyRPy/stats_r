#BIOL933
#Lab 1
#Script 1

#This script creates a new data set called SeedCount (a list) and calculates some basic statistics

SeedCount <- c(210,221,218,228,220,227,223,224,192)

length(SeedCount)
mean(SeedCount)
sd(SeedCount)
se(SeedCount)

se <- function(x) {sd(x)/sqrt(length(x))}
se(SeedCount)

CV <- function(x) {100 * sd(x)/mean(x)}
CV(SeedCount)