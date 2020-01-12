# Chapter 7 Auxiliary data and ratio estimation

# 7.8. COMPUTING NOTES ----------------------------------------------------

# date set - trees
# variable of interest - volume
# auxiliary variable - girth

y <- trees$Volume
x <- trees$Girth
s <- c(11, 4, 29, 27)
N <- 31
n <- 4

# ratio 
r <- mean(y[s])/mean(x[s])
r

mux <- mean(x)
mux
muhatr <- r * mux
muhatr

ssqr <- (1/(n-1))*sum((y[s] - r*x[s])^2)
ssqr

varhatmuhatr <- (1-n/N)*ssqr/n
se <- sqrt(varhatmuhatr)
se

qt(0.95, 3)
qt(0.05, 3)
qt(c(0.05, 0.95), 3)
b <- 6
plot(x[s], y[s])
plot(x, y)
plot(x, y, xlim = c(0, 22), ylim = c(0, 80))
for (k in 1:b){
  s <- sample(N, n)
}
muhatr <- numeric(b)
for (k in 1:b){
  s <- sample(N, n)
  muhatr[k] <- mean(y[s])/mean(x[s]) * mux
}

muhatr
mean(y)

for (k in 1:10000){
  s <- sample(N, n)
  muhatr[k] <- mean(y[s])/mean(x[s]) * mux
}
hist(muhatr)
hist(y)
hist(muhatr)
mean(muhatr) # 29.70

mu <- mean(y)
mu # 30.17

mean(muhatr - mu)
mean((muhatr - mu)^2)
var(muhatr)

for (k in 1:10000){
  s <- sample(N, n)
  muhatr[k] <- mean(y[s])/mean(x[s]) * mux
}
mean(muhatr - mu)
ybar <- numeric(b)

for (k in 1:10000){
  s <- sample(N, n)
  ybar[k] <- mean(y[s])
}

hist(ybar)
mean(ybar)
mean(ybar - mu)
var(ybar)

# histrogram of simple random sampling and sampling with ratio estimator
hist(ybar, freq = FALSE, ylim=c(0, 0.1), xlim = c(10, 60), col = "grey")
hist(muhatr, freq = FALSE, add = TRUE, col = "white")
