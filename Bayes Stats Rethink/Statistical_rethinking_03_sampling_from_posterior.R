
# 3.1. Sampling from a grid-approximate posterior -------------------------

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

# sample on p_grid, the prob of each value is posterior
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(samples)
library(rethinking)
dens(samples)

# 3.2. Sampling to summarize ----------------------------------------------
# intervals, for p < 0.5
sum(posterior[p_grid < 0.5])
sum(samples < 0.5) / 1e4

sum(samples > 0.5 & samples < 0.75) / 1e4

# quantiles
quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))

# PI and HPDI
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

PI(samples, prob = 0.5)
HPDI(samples, prob = 0.5)

# point estimate and concept of loss function
p_grid[ which.max(posterior)]
chainmode(samples, adj=0.01)

mean(samples)
median(samples)

sum(posterior*abs(0.5 - p_grid))

loss <- sapply(p_grid, function(d) sum(posterior*abs(d-p_grid)))
p_grid[which.min(loss)] # find the parameter value that minimize the loss


# 3.3. Sampling to simulate prediction ------------------------------------
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab="dummy water count")

w <- rbinom(1e4, size=9, prob=0.6)
hist(w)
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
