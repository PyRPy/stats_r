# basic theory of probability
# two coins simulation
A = rbinom(100000, 1, 0.5)
B = rbinom(100000, 1, 0.5)

mean(A | B) # 0.74885

# two coins simulation
A = rbinom(100000, 1, 0.5)
B = rbinom(100000, 1, 0.5)
C = rbinom(100000, 1, 0.5)

mean(A | B | C)

# Linear combinations -----------------------------------------------------

# constant coefficient
x = rbinom(100000, 10, 0.5)
mean(x)

y = 3 * x
mean(y)

# variance
var(x)
var(y)

# addition of two variables
X = rbinom(100000, 10, 0.5)
Y = rbinom(100000, 100, 0.3)

mean(X)
mean(Y)

# expected values
Z = X + Y
mean(Z)

# variance of X + Y, if X and Y are independent
var(Z)
