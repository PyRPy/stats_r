# Chapter 6 Point Estimation ----------------------------------------------

# 6.1 DESCRIPTIVE STATISTICS ----------------------------------------------

# Ex6.1-1 -----------------------------------------------------------------

dat <- scan("Data-R/E6_1-01.txt")
dat
# find sample mean
xbar <- mean(dat)
xbar

# sample variance
xvar <- var(dat)
xvar

# find sample standard deviation
xsd <- sd(dat)
xsd



# Ex6.1-3 -----------------------------------------------------------------

plungers <- scan("Data-R/E6_1-03.txt")

# a) sample mean and sd
mean(plungers)
sd(plungers)

# b) histograms
range(plungers)
length(plungers)
hist(plungers)
class_boundaries <- 10:22 + 0.95
class_boundaries

hist_obj <- hist(plungers, breaks = class_boundaries)
hist_obj$breaks
hist_obj$counts
