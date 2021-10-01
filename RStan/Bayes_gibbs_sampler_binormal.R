# bivariate normal
# https://www.mas.ncl.ac.uk/~ndjw1/teaching/sim/gibbs/gibbs.r
# first the "proper way"

# Produce data based on distributions -------------------------------------

rbvn<-function (n, rho) 
{
  x <- rnorm(n, 0, 1)
  y <- rnorm(n, rho * x, sqrt(1 - rho^2))
  cbind(x, y)
}

bvn<-rbvn(10000,0.98)
par(mfrow=c(3,2))
plot(bvn,col=1:10000)
plot(bvn,type="l")
plot(ts(bvn[,1]))
plot(ts(bvn[,2]))
hist(bvn[,1],40)
hist(bvn[,2],40)
par(mfrow=c(1,1))


# Using a Markov chain method ---------------------------------------------

# now with a gibbs sampler...

gibbs<-function (n, rho) 
{
  mat <- matrix(ncol = 2, nrow = n)
  x <- 0
  y <- 0
  mat[1, ] <- c(x, y)
  for (i in 2:n) {
    x <- rnorm(1, rho * y, sqrt(1 - rho^2))
    y <- rnorm(1, rho * x, sqrt(1 - rho^2))
    mat[i, ] <- c(x, y)
  }
  mat
}

bvn<-gibbs(10000,0.98)
par(mfrow=c(3,2))
plot(bvn,col=1:10000)
plot(bvn,type="l")
plot(ts(bvn[,1]))
plot(ts(bvn[,2]))
hist(bvn[,1],40)
hist(bvn[,2],40)
par(mfrow=c(1,1))
