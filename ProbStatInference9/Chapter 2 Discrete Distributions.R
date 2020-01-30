
# Chapter 2 Discrete Distributions ----------------------------------------
# Ref: Introduction to Probability and Statistics Using R by Jay Kerns
# basics and concepts
x <- c(0, 1, 2, 3)
f <- c(1/8, 3/8, 3/8, 1/8)

mu <- sum(x*f) # element wise
mu

# variance
sigma2 <- sum((x-mu)^2 * f)
sigma2

sigma <- sqrt(sigma2)
sigma

# accumulating probabilities
F <- cumsum(f)
F

# use package
library(distrEx)
X <- DiscreteDistribution(supp = 0:3, prob = c(1,3,3,1)/8)
E(X)
var(X)
sd(X)

# samples
s <- sample(6, size = 3000, replace = TRUE)
hist(s)

s <- sample(30:70, size = 27, replace = TRUE)
hist(s)

s <- sample(c("H", "T"), size = 1000, replace = TRUE)
table(s)
barplot(table(s))
