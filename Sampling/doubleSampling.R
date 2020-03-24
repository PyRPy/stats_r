# function for double sampling
doubleSampling <- function(N = 31, n1 = 10, n2 = 5, bb = 10){
  y <- trees$Volume
  x <- trees$Girth
  tauhat <- numeric(bb)
  tauhatr <- numeric(bb)
  
  for (k in 1:bb){
    s1 <- sample(1:N, n1)
    s2 <- sample(s1, n2)
    r <- mean(y[s2]) / mean(x[s2])
    tauhatr[k] <- r * mean(x[s1]) * N
    tauhat[k] <- N * mean(y[s2])
  }
  
  hist(tauhat, freq = F, ylim = c(0, 0.005))
  hist(tauhatr, add = T, col = "blue", freq = F)
  print(mean(tauhat))
  print(mean(tauhatr))
  print(length(tauhat))
  print(length(tauhatr))
}
