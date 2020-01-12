# write a function for simulation
srs <- function(y, n = 4, b = 10){
  ybar <- numeric(b)
  N <- length(y)
  for (k in 1:b){
    s <- sample(1:N, n)
    ybar[k] <- mean(y[s])
  }
  hist(ybar)
}