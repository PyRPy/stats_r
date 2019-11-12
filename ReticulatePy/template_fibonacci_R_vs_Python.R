# compare between R and Python on Fibonacci speed
# load packages and python envir
library(reticulate)
use_condaenv("deeplearning")

# Fibonacci in R
fibR <- function(n){
  if (n==0) return(0)
  if (n==1) return(1)
  return(fibR(n - 1) + fibR(n - 2))
}

# Fibnonacci in Python
source_python("fibpy.py")

# compare speed
library(rbenchmark)
benchmark(fibR(30), fibPy(30))
#        test replications elapsed relative user.self sys.self user.child sys.child
# 2 fibPy(30)          100   63.07     1.00     63.04     0.00         NA        NA
# 1  fibR(30)          100  110.98     1.76    110.36     0.56         NA        NA
