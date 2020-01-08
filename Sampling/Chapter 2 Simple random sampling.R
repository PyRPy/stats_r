# 2.8 Computing notes
set.seed(506)
y <- trees$Volume
N <- 31
n <- 10
s <- sample(1:N, n)
s

y[s]
mean(y[s])

s <- sample(1:N, n)
s
mean(y[s])
mu <- mean(y)
mu

b <- 6
ybar <- numeric(6)

# simulation of the sample mean
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}

ybar

b <- 10000
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)
mean(ybar)
var(ybar)
(1-n/N)*var(y)/n
sd(ybar)
sqrt((1-n/N)*var(y)/n)
mean((ybar - mu)^2)


# Example 4 - sampling data as a test population --------------------------
library(boot)
fir
head(fir)
y <- fir$count
y
table(y)

mean(y)

# Select a random sample without replacement
s <- sample(1:50, 5) # sample the index
s
y[s]

# calculate the sample mean
mean(y[s]) # true population mean is 2.14

s <- sample(1:50, 5); mean(y[s])

# run the multiple samplings
ybar <- numeric()
for (k in 1:10){s <- sample(1:50, 5); ybar[k] <- mean(y[s])}
ybar
hist(ybar)

# repeat 1000 times
ybar <- numeric(1000)
for (k in 1:1000){s <- sample(1:50, 5); ybar[k] <- mean(y[s])}
mean(ybar)
hist(ybar)

# poplulation mean 
mu <- mean(y)
mean(ybar) - mu
