# lab 1 challenges
biofuel_dat <- read.csv('Lab1ex3.csv', header = TRUE)
class(biofuel_dat)

# don't have to use 'as.data.frame'
# subsetting 1
biofuel_dat[c(4:6,16:18,28:30),]

# subsetting 2
sub_farm2 <- subset(biofuel_dat, Farm == 2)
sub_farm2

sub_farm2$Biomass

sub_biofuel <- subset(biofuel_dat, Ethanol >= 4400)
sub_biofuel

sub2_biofuel <- subset(biofuel_dat, Ethanol >= 4400 & Biomass < 90)
sub2_biofuel

# subsetting 3
min_4400 <- biofuel_dat$Ethanol >= 4400
biofuel_dat[min_4400,c(1,2,4)]