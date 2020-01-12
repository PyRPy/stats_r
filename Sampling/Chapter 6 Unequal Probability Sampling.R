# Chapter 6 Unequal Probability Sampling

# 6.6. COMPUTING NOTES ----------------------------------------------------

head(trees)
y <- trees$Volume
N <- 31
summary(trees)

# probability
p <- trees$Girth
p <- p/sum(p)
sum(p) # sum(p) = 1

# select a sample with probability proportional to girth with replacement
n <- 10
s <- sample(1:N, n, replace = TRUE, prob = p)
s
y[s]
mean(y[s]) # sample
mu <- mean(y) # population
mu

# simulate sample mean
ybar <- numeric(6)
for (k in 1:6){
  s <- sample(1:N, n, replace = TRUE, prob = p)
  ybar[k] <- mean(y[s])
}

for (k in 1:6) {
  s <- sample(1:N, n, replace = TRUE, prob = p)
  ybar[k] <- mean(y[s])
}

ybar

# a full simulation
b <- 10000
for (k in 1:b) {
  s <- sample(1:N, n, replace = TRUE, prob = p)
  ybar[k] <- mean(y[s])
}

mean(ybar)
hist(ybar)
points(mu, 0)


# Hansen-Hurwitz estimator ------------------------------------------------

hh <- mean(y[s]/p[s])/N
s
hh
hh <- numeric(0)
for (k in 1:b) {
  s <- sample(1:N, n, replace = TRUE, prob = p)
  hh[k] <- mean(y[s]/p[s])/N
}
mean(hh)
mu
hist(hh)
points(mu, 0)
hist(ybar)
points(mu, 0)
hist(hh, add=T, col="blue")


# compare sample mean with simple random sampling -------------------------

ybarSRS <- numeric(0)
for (k in 1:b){
  s <- sample(1:N, n)
  ybarSRS[k] <- mean(y[s])
}
hist(ybarSRS, add=FALSE, col = "yellow")
hist(hh, add=TRUE, col = "blue")


# unequal prob sampling with HT estimator ---------------------------------

s
unique(s)
hh <- mean(y[s]/p[s])
hh

# inclusion probabilities
pii <- 1 - (1 - p)^n
pii
hh
ht <- sum(y[unique(s)]/pii[unique(s)])
unique(s)
ht
su <- unique(s)
s
su
y[s]
y[su]
ht <- sum(y[su]/pii[su])
ht

# in a loop
b <- 6
for (k in 1:b){
  s <- sample(1:N, n, T, p)
  su <- unique(s)
  hh[k] <- mean(y[s]/p[s])
  ht[k] <- sum(y[su]/p[su])
}

# do a full simulation
b <- 10000
for (k in 1:b){
  s <- sample(1:N, n, T, p)
  su <- unique(s)
  hh[k] <- mean(y[s]/p[s])
  ht[k] <- sum(y[su]/pii[su]) # find the error !
}

tau <- sum(y)
tau
mean(hh)
mean(ht)
hist(hh)
hist(ht)
mean((hh-tau)^2)
mean((ht - tau)^2)

hist(ht, freq = F, ylim= c(0, 0.005))
hist(hh, add=T, freq = F, col = "blue")
