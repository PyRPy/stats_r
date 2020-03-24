# Chapter 14 Double sampling

# 14.6 Computing notes ----------------------------------------------------

# data
trees
names(trees)

# set variable of interest and auxilliary variance
y <- trees$Volume
x <- trees$Girth

# check correlation visually
plot(x, y)

# start double-sampling
set.seed(506)
s1 <- sample(1:31, 10) # first x
s1
s2 <- sample(s1, 5) # second y
s2

plot(x[s2], y[s2]) # still looks linear

mean(x[s1])
mean(x)
mean(y[s2]) / mean(x[s2])
sum(y[s2]) / sum(x[s2])

# ratio
r <- mean(y[s2]) / mean(x[s2])
r

# total estimate
tauhatr <- r * mean(x[s1]) * nrow(trees)
tauhatr # 962.2541
sum(y) # 935.3

# run simulation for distribution check
for (k in 1:10){
  s1 <- sample(1:31, 10)
  s2 <- sample(s1, 5)
  r <- mean(y[s2]) / mean(x[s2])
  tauhatr[k] <- r * mean(x[s1]) * 31
}

tauhatr

# run more tests and check histogram
for (k in 1:1000){
  s1 <- sample(1:31, 10)
  s2 <- sample(s1, 5)
  r <- mean(y[s2]) / mean(x[s2])
  tauhatr[k] <- r * mean(x[s1]) * 31
}

hist(tauhatr)

tauhat <- 31 * mean(y[s2])
tauhat <- numeric(0)
for (k in 1:1000){
  s1 <- sample(1:31, 10)
  s2 <- sample(s1, 5)
  r <- mean(y[s2]) / mean(x[s2])
  tauhatr[k] <- r * mean(x[s1]) * 31
  tauhat[k] <- mean(y[s2])
}
hist(tauhat)
hist(tauhatr, add = T, col = "blue")

## somehow, hist won't overlap...

# mean and variance estimates
N <- 31
n1 <- 10
n2 <- 5
for (k in 1:1000){
  s1 <- sample(1:N, n1)
  s2 <- sample(s1, n2)
  r <- mean(y[s2]) / mean(x[s2])
  tauhatr[k] <- r * mean(x[s1])
  tauhat[k] <- N * mean(y[s2])
}
hist(tauhat, freq = F, ylim = c(0, 0.01))
hist(tauhatr, add=T, col = "blue", freq = F)

mean(tauhat) # 935.2167
mean(tauhatr) # 841.7229

mean(y)
sum(y)

tau <- sum(y)
mean((tauhat - tau)^2)
var(tauhat)
mean((tauhatr - tau)^2)
msetauhat <- mean((tauhat - tau)^2)
msetauhatr <- mean((tauhatr - tau)^2)
sqrt(msetauhat)
sqrt(msetauhatr) # ratio is worse ?