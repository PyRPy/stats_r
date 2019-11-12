#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int fibC(const int x){
  if (x == 0)
    return(0);
  if (x == 1)
    return(1);
  return (fibC(x - 1) + fibC(x - 2));
}

