

# Simple linear regression using BUGS -------------------------------------

# http://rstudio-pubs-static.s3.amazonaws.com/1935_652d6fb17d7941c4b91bbbc5a22d4494.html

library(R2OpenBUGS)

# model
linemodel <- function() {
  for (j in 1:N) {
    Y[j] ~ dnorm(mu[j], tau)
    mu[j] <- alpha + beta * (x[j] - xbar)
  }

  # priors
  alpha ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1 / sqrt(tau)
}


# data
Y = c(1, 3, 3, 3, 5)
x = c(1, 2, 3, 4, 5)
plot(x, Y)
linedata <- list(Y = Y, x = x, N = 5, xbar = 3)

# initials
lineinits <- function() {
  list(alpha = 1, beta = 1, tau = 1)
}

# run BUGS model for sampling
lineout <- bugs(data=linedata,
                inits = lineinits,
                parameters.to.save = c("alpha", "beta", "sigma"),
                model.file = linemodel,
                n.chains = 1,
                n.iter = 10000,
                debug = TRUE)

# print out result
lineout

# multiple chains
lineinits <- function() {
  list(alpha = rnorm(1, 0, 1),
       beta = rnorm(1, 0, 1),
       tau = runif(1, 0, 1))
}

lineout <- bugs(data=linedata,
                inits = lineinits,
                parameters.to.save = c("alpha", "beta", "sigma"),
                model.file = linemodel,
                n.chains = 1,
                n.iter = 10000,
                debug = TRUE)

lineout$summary

# find quantiles
names(lineout$sims.list)
quantile(lineout$sims.list[[1]], probs = c(0.025, 0.975))
quantile(lineout$sims.list[[2]], probs = c(0.025, 0.975))
quantile(lineout$sims.list$beta, probs = c(0.025, 0.975))
