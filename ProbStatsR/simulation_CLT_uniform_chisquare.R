
# Simulation for CLT for uniform and chi-square distributions -------------
# https://online.stat.psu.edu/stat414/lesson/27/27.2
# create a population with uniform distribution
N = 100000
population_uniform <- runif(N, min=0, max = 1)
population_chisquare <- rchisq(N, 3) # df = 3



# Uniform dstribution -----------------------------------------------------
n <- 1
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_uniform, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 2
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_uniform, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 5
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_uniform, n)
  ybar[k] <- mean(s)
}

hist(ybar)


# Chi-Square distribution -------------------------------------------------

n <- 1
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_chisquare, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 2
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_chisquare, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 5
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_chisquare, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 10
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_chisquare, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 20
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_chisquare, n)
  ybar[k] <- mean(s)
}

hist(ybar)

n <- 30
ybar <- numeric()
for (k in 1:1000) {
  s <- sample(population_chisquare, n)
  ybar[k] <- mean(s)
}

hist(ybar)
