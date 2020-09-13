# Probability distributions -----------------------------------------------
# https://en.wikipedia.org/wiki/Binomial_distribution
# https://en.wikipedia.org/wiki/Bernoulli_distribution
# https://en.wikipedia.org/wiki/Poisson_distribution
# https://en.wikipedia.org/wiki/Geometric_distribution
# https://en.wikipedia.org/wiki/Hypergeometric_distribution
# https://en.wikipedia.org/wiki/Negative_binomial_distribution


# bernoulli distribution --------------------------------------------------

dbern(10, 0.5)

require(graphics)
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)
## extreme points are omitted since dbinom gives 0.
mtext("dbinom(k, log=TRUE)", adj = 0)
mtext("extended range", adj = 0, line = -1, font = 4)
mtext("log(dbinom(k))", col = "red", adj = 1)
