# Very common distributions -----------------------------------------------
# https://www.tutorialspoint.com/r/r_binomial_distribution.htm

# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)

# Create the binomial distribution.
y <- dbinom(x,50,0.5)

# Plot the graph for this sample.
plot(x,y)

# https://stackoverflow.com/questions/32705890/how-to-plot-an-exponential-distribution
x <- seq(0, 20, length.out=1000)
dat <- data.frame(x=x, px=dexp(x, rate=0.65))
library(ggplot2)
ggplot(dat, aes(x=x, y=px)) + geom_line()

# even more simplified
curve(0.65*exp(-0.65*x), from=0, to=10)


# Bernolli ----------------------------------------------------------------

library(Rlab)
x <- seq(0,10,by = 1)
y = dbern(x, prob = 0.5, log = FALSE)
plot(x, y)


# Piosson -----------------------------------------------------------------
# http://statweb.stanford.edu/~susan/courses/s141/Rlab2sol/
x <- c(0,1,2,5,8,10,15,20)
ppois(x,6)

# https://astrostatistics.psu.edu/su07/R/html/stats/html/Poisson.html
x <- seq(-0.01, 5, 0.01)
plot(x, ppois(x, 1), type="s", ylab="F(x)", main="Poisson(1) CDF")
