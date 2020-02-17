
# Binomial distribution ---------------------------------------------------

a <- 6
ifelse(rbinom(4, 1, 0.5), -a, a)

# another method
n <- 10
X <- rep(0, n)
Y <- rbinom(n, 1, 1/2)
X[Y==1] = "a"
X[Y==0] = "-a"
X
Y
# ref https://stackoverflow.com/questions/22893089/r-how-to-generate-random-sample-of-a-discrete-random-variables
