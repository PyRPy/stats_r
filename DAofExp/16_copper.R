# copper.r, acid copper pattern plating experiment, Tables 16.17-18, pp602-603

# Read first 6 observations from file
copper.data <- head(read.table("Data/copper.txt", header=T), 6)

# Code data
# install.packages("rsm")
library(rsm)
copper1 <- coded.data(copper.data, zA ~ xA - 10.5, zB ~ (xB - 36)/5 )
copper1

# First-order model analysis
model1 <- rsm(s ~ FO(zA, zB), data=copper1)
summary(model1)
steepest(model1, dist=seq(0, 5, by=1), descent=F)
