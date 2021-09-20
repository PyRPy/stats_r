
# Bayes for proportion ----------------------------------------------------
# Ref: Applied Bayesian Statistics With R and OpenBUGS Examples
# 3.6.2 Summarizing and Graphing Probability Distributions in R

# CI for proportion
qbeta(c(0.025, 0.975), 10, 40) # 0.1024494 0.3202212

# curves for proportions space
x <- seq(0.005,0.995,length=100)
y <- dbeta(x, 10, 40)
plot(x,y,type="l")

plot(x, y, type="l", xlab=expression(pi),
     ylab="Density",
     main = "Beta(10,40)")

# normal distribution
x <- seq( qnorm( 0.005, 20,5 ), qnorm( 0.995, 20, 5),
          length = 100)
y <- dnorm( x, 20, 5)
plot( x,y, type="l")


# 3.6.4.1 LearnBayes ------------------------------------------------------

library(LearnBayes)
triplot( c(2.5, 10), c(7,43) )
