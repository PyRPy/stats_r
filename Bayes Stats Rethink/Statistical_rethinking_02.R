
# 2.3 Components of the model ---------------------------------------------

# The counts of “water” W and “land’ L are distributed binomially,
# with probability p of “water” on each toss.

# likelihood
dbinom(6, size = 9, prob = 0.5)


# 2.4.3. Grid approximation. ----------------------------------------------

# define grid
p_grid <- seq(from=0, to=1, length.out = 20)

# prior
# prior <- rep(1, 20)
prior <- ifelse(p_grid < 0.5, 0, 1)

# prior <- exp(-5*abs(p_grid - 0.5))

# likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, sum it to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# plot for comparison
plot(p_grid, posterior, type = "b",
     xlab="probability of water",
     ylab = "posterior probability")
mtext("20 points")


# 2.4.4. Quadratic approximation. -----------------------------------------
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p),
    p ~ dunif(0, 1)
  ),
  data=list(W=6, L=3)
)

# summary
precis(globe.qa)

# compare with beta distribution
W <- 6
L <- 3
curve(dbeta(x, W+1, L+1), from = 0, to=1)
curve(dnorm(x, 0.67, 0.16), lty=2, add = TRUE)

# Monte Carlo Globe Tossing

n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
  p_new <- rnorm( 1 , p[i-1] , 0.1 )
  if ( p_new < 0 ) p_new <- abs( p_new )
  if ( p_new > 1 ) p_new <- 2 - p_new
  q0 <- dbinom( W , W+L , p[i-1] )
  q1 <- dbinom( W , W+L , p_new )
  p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}

dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )
