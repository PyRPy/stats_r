# compare between R and Python on Fibonacci speed
# load packages and python envir
library(reticulate)
use_condaenv("deeplearning")
library(Rcpp)

# Fibonacci in R
fibR <- function(n){
  if (n==0) return(0)
  if (n==1) return(1)
  return(fibR(n - 1) + fibR(n - 2))
}

# Fibonacci in Python
source_python("fibpy.py")

# Fibonacci in C ++
Rcpp::sourceCpp('fibRcpp.cpp')

# compare speed
library(rbenchmark)
benchmark(fibR(30), fibPy(30))
#        test replications elapsed relative user.self sys.self user.child sys.child
# 2 fibPy(30)          100   63.07     1.00     63.04     0.00         NA        NA
# 1  fibR(30)          100  110.98     1.76    110.36     0.56         NA        NA

benchmark(fibR(30), fibPy(30), fibC(30))
#        test replications elapsed relative user.self sys.self user.child sys.child
# 3  fibC(30)          100    0.43    1.000      0.43     0.00         NA        NA
# 2 fibPy(30)          100   63.60  147.907     63.58     0.00         NA        NA
# 1  fibR(30)          100  111.63  259.605    110.85     0.75         NA        NA