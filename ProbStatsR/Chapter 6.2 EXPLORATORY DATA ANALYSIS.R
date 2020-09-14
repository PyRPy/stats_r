# 6.2 EXPLORATORY DATA ANALYSIS -------------------------------------------

# Ex6.2-1 -----------------------------------------------------------------

plungers <- scan("Data-R/E6_1-03.txt")
plungers

# a) Stem and leaf
stem(plungers)

# b) five number summary
summary(plungers)

# c) boxplot, outliers ?
boxplot(plungers)

plungers_sorted <- sort(plungers)
# outliers 
tail(plungers_sorted, 2) # 21.5 22.1, two largest ones
head(plungers_sorted, 1) # 11.9, smallest one 
# three outliers total


# Ex6.2-3 -----------------------------------------------------------------

# underwater weights

weights <- read.delim("Data-R/E6_2-03.txt", header=FALSE)
names(weights) <- c("male", "female")
# a) group by marks 
marks <- 0:6 + 0.5
summary(weights)  

# b) histograms
hist(weights$male, breaks = marks)
hist(weights$female, breaks = marks, col = "blue", add=TRUE)

# box-and-whisker diagrams 
library(reshape2)

weights_long <- na.omit(melt(weights))
names(weights_long) <- c("gender", "weight")
weights_long$gender <- as.factor(weights_long$gender)
boxplot(weight ~ gender, data = weights_long, main="Underwater weights")
