# binomial distribution ---------------------------------------------------

n_flipps = 1000
rbinom(n_flipps, 1, 0.5)
res = rbinom(n_flipps, 1, 0.5)
sum(res)/length(res)

# simulations
n_flipps = 100000
res = rbinom(n_flipps, 10, 0.5)
hist(res)

mean(res == 5) # 0.24626

dbinom(5, 10, 0.5) # 0.2460938

mean(res <=4) # 0.37649
pbinom(4, 10, 0.5) # 0.3769531

# expected value
mean(res)
mean(rbinom(100000, 10, 0.2)) # mu = n * p 

# variance
var(res) # var = n * p * (1-p)

# simulating two events
# independent, multiplication rule
A = rbinom(100000, 1, 0.5)
B = rbinom(100000, 1, 0.5)
A & B
mean(A & B) # 0.24928

A = rbinom(100000, 1, 0.2)
B = rbinom(100000, 1, 0.5)
A & B
mean(A & B) # 0.09947
