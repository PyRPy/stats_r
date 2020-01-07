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
