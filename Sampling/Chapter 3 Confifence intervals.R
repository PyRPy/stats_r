# Chapter 3 Confifence intervals
choose(31, 10)


# 3.4 Computing notes -----------------------------------------------------

# Confidence Interval Computation
y <- c(1, 50, 21, 98, 2, 36, 4, 29, 7, 15, 86, 10, 21, 5, 4)
n <- length(y)
n

N <- 286
ybar <- mean(y)
ybar
var_hat_y_bar <- (1-n/N) * var(y)/n
var_hat_y_bar

tau_hat <- N*ybar
tau_hat

var_hat_tau_hat <- N^2 * var_hat_y_bar
var_hat_tau_hat

se_tau_hat <- sqrt(var_hat_tau_hat)
se_tau_hat

qt(0.95, df = 14) # 1.76131

LowerLimit <- tau_hat - qt(0.95, df=14) * se_tau_hat
UpperLimit <- tau_hat + qt(0.95, df=14) * se_tau_hat
cat(LowerLimit, UpperLimit)

qt(c(0.05, 0.95), df =14)
CI <- tau_hat + qt(c(0.05, 0.95), df = 14) * se_tau_hat # vector
CI

# Approximate Normality of a Sampling -------------------------------------
# illustrate the finite-population central limit theorem
y <- trees$Volume
N <- 31
hist(y)
hist(y, xlim = c(10, 80))
ybar <- numeric(0)
b = 1000
n = 1 # number of sample 1
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)

n = 2 # number of sample 2
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)

n = 5 # number of sample 5
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)

n = 15 # number of sample 15
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)

n = 25 # number of sample 25
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)

n = 30 # number of sample 30
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar)

# width of the histogrms held constant
n = 1 # number of sample 1
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar, xlim = c(10, 80))

n = 2 # number of sample 2
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar, xlim = c(10, 80))

n = 5 # number of sample 5
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar, xlim = c(10, 80))

n = 15 # number of sample 15
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar, xlim = c(10, 80))

n = 25 # number of sample 25
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar, xlim = c(10, 80))

n = 30 # number of sample 30
for (k in 1:b){
  s <- sample(1:N, n)
  ybar[k] <- mean(y[s])
}
hist(ybar, xlim = c(10, 80)) # narrower and narrower


# Daily Precipitation Data ------------------------------------------------
weather <- read.table(file="http://www.stat.sfu.ca/???thompson/data/weather.txt")
head(weather)
# write.csv(weather, "weather.csv")
names(weather)
y <- weather$V9
y
hist(y, breaks = 20)

plot(1:90, y)
lines(1:90, y)
qqnorm(y)

plot(ecdf(y))
plot(density(y)) # not normal at all

# simulations
s <- sample(1:90, 3); mean(y[s])
ybar <- numeric()
ybar
for (k in 1:10){
  s <- sample(1:90, 3)
  ybar[k] <- mean(y[s])
}
hist(ybar)

# n = 3
for (k in 1:10000){
  s <- sample(1:90, 3)
  ybar[k] <- mean(y[s])
}
hist(ybar)
qqnorm(ybar)

# n = 15
for (k in 1:10000){
  s <- sample(1:90, 15)
  ybar[k] <- mean(y[s])
}
hist(ybar)
qqnorm(ybar)

# n = 45
for (k in 1:10000){
  s <- sample(1:90, 45)
  ybar[k] <- mean(y[s])
}
hist(ybar)
qqnorm(ybar)

# n = 87
for (k in 1:10000){
  s <- sample(1:90, 87)
  ybar[k] <- mean(y[s])
}
hist(ybar)
qqnorm(ybar)