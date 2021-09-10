# Bayesian models in R ----------------------------------------------------
# https://www.r-bloggers.com/2019/05/bayesian-models-in-r-2/

# Step 1. All possible ways (likelihood distribution)

rangeP <- seq(0, 1, length.out = 100)
plot(rangeP, dbinom(x = 8, prob = rangeP, size = 10),
     type = "l", xlab = "P(Black)", ylab = "Density")

# Step 2. Update your belief (prior distribution)
lines(rangeP, dnorm(x = rangeP, mean = .5, sd = .1) / 15,
      col = "red")

lik <- dbinom(x = 8, prob = rangeP, size = 10)
prior <- dnorm(x = rangeP, mean = .5, sd = .1)
lines(rangeP, lik * prior, col = "green")

# Step 3. Make it sum up to one (standardising the posterior)
unstdPost <- lik * prior
stdPost <- unstdPost / sum(unstdPost)
lines(rangeP, stdPost, col = "blue")
legend("topleft", legend = c("Lik", "Prior", "Unstd Post", "Post"),
       text.col = 1:4, bty = "n")


# Simulation --------------------------------------------------------------
# Take a sample of 100 observations from the distribution Normal(5, 2) .
# Define real pars mu and sigma, sample 100x
trueMu <- 5
trueSig <- 2
set.seed(100)
randomSample <- rnorm(100, trueMu, trueSig)

# Grid approximation, mu %in% [0, 10] and sigma %in% [1, 3]
grid <- expand.grid(mu = seq(0, 10, length.out = 200),
                    sigma = seq(1, 3, length.out = 200))

# Compute likelihood
lik <- sapply(1:nrow(grid), function(x){
  sum(dnorm(x = randomSample, mean = grid$mu[x],
            sd = grid$sigma[x], log = T))
})

# Multiply (sum logs) likelihood and priors
prod <- lik + dnorm(grid$mu, mean = 0, sd = 5, log = T) +
  dexp(grid$sigma, 1, log = T)

# Standardize the lik x prior products to sum up to 1, recover unit
prob <- exp(prod - max(prod))

# Sample from posterior dist of mu and sigma, plot
postSample <- sample(1:nrow(grid), size = 1e3, prob = prob)
plot(grid$mu[postSample], grid$sigma[postSample],
     xlab = "Mu", ylab = "Sigma", pch = 16, col = rgb(0,0,0,.2))
abline(v = trueMu, h = trueSig, col = "red", lty = 2)
