# Gibbs sampler in R ------------------------------------------------------
# https://adv-r.hadley.nz/rcpp.html
gibbs_r <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- 0
  y <- 0
  
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}


# Run the cpp code to generate function -----------------------------------
library(Rcpp)
sourceCpp('Bayes_gibbs_sampler_cpp.cpp')

library(microbenchmark)
microbenchmark(gibbs_cpp(100, 10), gibbs_r(100, 10))
