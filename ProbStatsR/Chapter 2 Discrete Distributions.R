# Ex2.2-1 -----------------------------------------------------------------

X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
sum(X^2)/55 #7

# Ex2.6-1 
# b) 
# https://stackoverflow.com/questions/7906332/how-to-calculate-combination-and-permutation-in-r
perm = function(n, x) {
  factorial(n) / factorial(n-x)
}

comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}

prob <- comb(29, 2) * 0.1^3 * 0.9^27
prob # 0.02360879

