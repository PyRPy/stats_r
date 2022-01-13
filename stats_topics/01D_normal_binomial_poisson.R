# normal distribution -----------------------------------------------------

# flip 10 coins
flips = rbinom(100000, 10, 0.5)
hist(flips)

# flip 1000 coins
flips = rbinom(100000, 1000, 0.5)
hist(flips)

# approximate binomial by normal
binomial = rbinom(100000, 1000, 0.5)
hist(binomial)

normal = rnorm(100000, 1000 * 0.5, sqrt(1000*0.5*(1-0.5)))
hist(normal)


# poisson distribution ----------------------------------------------------

# for rare events, like flipping many coins, but with low prob
binomial_low_prob = rbinom(100000, 1000, 1/1000)
hist(binomial_low_prob)

# compare poisson with binomial
poisson_1 = rpois(100000, 1) # mu = lambda = 1000 * 1/1000 = 1 in binom
hist(poisson_1)


# geometric - waiting for happening ---------------------------------------

flips = rbinom(100, 1, 0.1)
flips
which(flips == 1)
which(flips == 1)[1]

# repeat simulations
replicate(10, which(rbinom(100, 1, 0.1) == 1)[1])

# histograms
geom = rgeom(100000, 0.1)
mean(geom) # E(X) = 1/p - 1
