# compare between R, Python and C++ on Fibonacci speed
# load packages and python envir
library(reticulate)
# use_condaenv("deeplearning")
library(Rcpp)

# Fibonacci in R
fibR <- function(n){
  if (n==0) return(0)
  if (n==1) return(1)
  return(fibR(n - 1) + fibR(n - 2))
}

# Fibonacci in Python
source_python("fibpy.py")

# Fibonacci in python with dynamic programming
source_python("fibonacciDynamic.py")

# Fibonacci in C ++
Rcpp::sourceCpp('fibRcpp.cpp')

# compare speed
library(rbenchmark)
benchmark(fibC(30), fibPyD(30))

#         test replications elapsed relative user.self sys.self user.child sys.child
# 1   fibC(30)          100   0.543   20.885     0.537    0.003          0         0
# 2 fibPyD(30)          100   0.026    1.000     0.026    0.000          0         0