# Course Handouts for Bayesian Data Analysis Class ------------------------

# https://bookdown.org/marklhc/notes_bookdown/

library(tidyverse)
# Chapter 1 Introduction --------------------------------------------------
set.seed(4)
truly_drunk <- c(rep("drunk", 100), rep("sober", 100 * 999))
table(truly_drunk)

# Chapter 2 Bayesian Inference --------------------------------------------

data("Aids2", package = "MASS")
set.seed(15)
Aids2_sub <- Aids2 %>% sample_n(10)
Aids2_sub

# install.packages("psych")  # uncomment to install the `psych` package
library(psych)
pairs.panels(Aids2_sub, ellipses = FALSE)

# research question:
# What is the death rate of AIDS in Australia when the data were collected?

## PMF
par(mfrow = c(2, 2), mar = c(4, 4, 2, 0) + 0.1)
for (th in c(0.1, 0.2, 0.5, 0.8)) {
  plot(0:10, dbinom(0:10, 10, th), type = "h", xlab = "y",
       ylab = expression(italic(P)(Y == y)), bty = "L",
       lwd = 2)
  pos <- "topright"
  if (th > 0.6) pos <- "topleft"
  legend(pos, legend = bquote(theta == .(th)))
}

# ggplot version
th <- c(0.1, 0.2, 0.5, 0.8)
plist <- vector("list", length = length(th))
for (i in 1:4) {
  label_xpos <- if (th[i] > .6) 2 else 8
  plist[[i]] <- ggplot(tibble(y = 0:10, 
                              prob = dbinom(0:10, 10, th[i])), 
                       aes(x = y, y = prob)) + 
    geom_bar(stat = "identity", width = 0.1) + 
    labs(y = expression(italic(P)(Y == y)), 
         x = expression(y)) + 
    scale_x_continuous(breaks = 0:10) + 
    annotate("text", x = label_xpos, y = Inf, vjust = 1, 
             label = list(bquote(theta == .(th[i]))), 
             parse = TRUE)
}
gridExtra::grid.arrange(grobs = plist, nrow = 2)

## likelihood
# Probability of y = 6 for 10 trials, with theta = 0.5
dbinom(6, 10, 0.5)

th <- seq(0, 1, by = 0.1)
tibble(theta = th, 
       likelihood = dbinom(6, 10, prob = th))

# plot the likelihood curve
ggplot(tibble(x = c(0, 1)), 
       aes(x = x)) + 
  stat_function(fun = dbinom, args = list(x = 6, size = 10), col = "red") + 
  labs(x = expression(theta), 
       y = "likelihood")

## Specifying Priors
# brute-force method 
# Define a grid for the parameter

th_grid <- seq(0, 1, by = 0.01)
# Get the prior density for each value on the grid
prior <- dbeta(th_grid, 46, 34)
# Get the likelihood for each value on the grid
lik <- dbinom(6, 10, prob = th_grid)
# Multiply to get the posterior
post_raw <- prior * lik
# Scale it back to density so that the sum is 1
post_dens <- post_raw / sum(post_raw * 0.01)
# Print a table for a few values
grid_dat <- tibble(theta = th_grid, 
                   prior = prior, 
                   likelihood = lik, 
                   `prior x likelihood` = post_raw, 
                   posterior = post_dens) 
grid_dat[51:60, ]

# par(mfrow = c(1, 2), mar = c(4, 4, 2, 0) + 0.1)
# # Plot the prior
# plot(th_grid, prior, type = "o", col = "green3", ylim = c(0, 5), 
#      ylab = "density", cex = 0.7, xlab = expression(theta))
# # Plot the likelihood (need to scale it)
# lines(th_grid, lik / sum(lik) / 0.05, type = "o", col = "red", cex = 0.7)
# # Plot the post raw (need to scale it)
# lines(th_grid, post_dens, type = "o", col = "blue", cex = 0.7)
# legend("topright", c("Prior", "Likelihood", "Posterior"), 
#        lwd = 1, 
#        col = c("green3", "red", "blue"), cex = 0.7)
# curve(dbeta(x, 52, 38), ylim = c(0, 5), 
#       ylab = "density", xlab = expression(theta), col = "blue")
# text(0.6, 4, "Beta(52, 38)")
p1 <- ggplot(grid_dat, aes(x = theta)) + 
  geom_point(aes(y = prior, col = "Prior")) + 
  geom_line(aes(y = prior, col = "Prior")) + 
  geom_point(aes(y = likelihood / sum(likelihood) / 0.01, col = "Likelihood")) + 
  geom_line(aes(y = likelihood / sum(likelihood) / 0.01, col = "Likelihood")) + 
  geom_point(aes(y = posterior, col = "Posterior")) + 
  geom_line(aes(y = posterior, col = "Posterior")) + 
  labs(x = expression(theta), y = "Density", 
       col = "") + 
  ylim(0, 8) + 
  scale_color_manual("", values = c("red", "blue", "green3")) + 
  theme(legend.position = c(0.80, 0.80))
p2 <- ggplot(grid_dat, aes(x = theta)) + 
  stat_function(fun = dbeta, args = list(shape1 = 52, shape2 = 38), 
                col = "blue") + 
  geom_text(x = 0.2, y = 7, label = "Beta(52, 38)") + 
  labs(x = expression(theta), y = "Density", 
       col = "") + 
  ylim(0, 8)
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Laplace approximation
log_prior <- function(th) dbeta(th, 46, 34, log = TRUE)
log_lik <- function(th) dbinom(6, 10, th, log = TRUE)
log_posterior <- function(th) log_prior(th) + log_lik(th)

# Find the MAP estimate with the Hessian
fit <- optim(runif(1, 0, 1), log_posterior, method = "L-BFGS-B", 
             lower = 1e-5, upper = 1 - 1e-5, 
             control = list(fnscale = -1), 
             hessian = TRUE)
fit$par  # MAP
1 / sqrt(-fit$hessian[1])  # estimate of posterior standard deviation

# normal approximation of the posterior distribution

# par(mar = c(4, 4, 2, 0) + 0.1)
# curve(dbeta(x, 52, 38), xlab = expression(theta), ylab = "density")
# curve(dnorm(x, fit$par, 1 / sqrt(-fit$hessian)), add = TRUE, 
#       col = "blue", lty = "dashed")
# legend("topright", c("Beta(52, 38)", "Normal approximation"), 
#        lty = c("solid", "dashed"), col = c("black", "blue"))

ggplot(tibble(x = c(0, 1)), aes(x = x)) + 
  stat_function(fun = dbeta, args = list(shape1 = 52, shape2 = 38), 
                aes(col = "Beta(52, 38)", linetype = "Beta(52, 38)"), 
                size = 2) + 
  stat_function(fun = dnorm, args = list(mean = fit$par, 
                                         sd = 1 / sqrt(-fit$hessian[1])), 
                aes(col = "Normal approximation", 
                    linetype = "Normal approximation")) + 
  labs(x = expression(theta), y = "Density", 
       col = "", linetype = "") + 
  scale_color_manual("", values = c("skyblue", "blue")) + 
  scale_linetype_manual("", values = c("dashed", "solid"))

# Markov Chain Monte Carlo (MCMC)
# This is called the Metropolis algorithm
log_prior_kernel <- function(th) (46 - 1) * log(th) + (34 - 1) * log(1 - th)
log_lik_kernel <- function(th, y = 6, n = 10) y * log(th) + 
  (n - y) * log(1 - th)
log_posterior_kernel <- function(th, y = 6, n = 10) {
  log_prior_kernel(th) + log_lik_kernel(th, y, n)
}
post <- rep(NA, 1e4 + 1)
post[1] <- runif(1)
for (i in seq_len(length(post) - 1)) {
  proposal <- rnorm(1, post[i], 0.1)
  if (proposal > 1 | proposal < 0) {
    post[i + 1] <- post[i]
    next
  }
  p_accept <- exp(log_posterior_kernel(proposal) - log_posterior_kernel(post[i]))
  post[i + 1] <- ifelse(runif(1) < p_accept, proposal, post[i])
}
# Discard the burn-in
post <- post[-(1:2000)]
# # Plot the posterior samples
# par(mfrow = c(1, 2), mar = c(4, 4, 2, 0) + 0.1)
# plot.ts(post[1:500], xlab = "iterations", ylab = expression(theta))
# plot(density(post, bw = "SJ"), xlab = expression(theta), main = "")
p1 <- ggplot(tibble(iter = 1:500, th = post[1:500]), 
             aes(x = iter, y = th)) + 
  geom_line() + 
  labs(x = "iterations", y = expression(theta))
p2 <- ggplot(tibble(th = post), aes(x = th)) + 
  geom_density(bw = "SJ") + 
  labs(x = expression(theta)) + 
  xlim(0, 1)
gridExtra::grid.arrange(p1, p2, nrow = 1)

# posterior point estimates comparison
# Grid approximation:
median_llpt <- max(which(cumsum(post_dens) < sum(post_dens) / 2))
median_interplolate <- (sum(post_dens) / 2 - cumsum(post_dens)[median_llpt]) / 
  (diff(cumsum(post_dens)[median_llpt + 0:1]))
bayes_est_grid <- c(mean = mean(post_dens * th_grid), 
                    median = median_interplolate * 0.01 + 
                      th_grid[median_llpt], 
                    mode = th_grid[which.max(post_dens)])
# Conjugate prior
bayes_est_conj <- c(mean = 52 / (52 + 38), 
                    median = qbeta(.50, 52, 38), 
                    mode = (52 - 1) / (52 + 38 - 2))
# Laplace approximation
bayes_est_laplace <- c(mean = fit$par, 
                       median = fit$par, 
                       mode = fit$par)
# MCMC
mcmc_dens <- density(post, bw = "SJ")
bayes_est_mcmc <- c(mean = mean(post), 
                    median = median(post), 
                    mode = mcmc_dens$x[which.max(mcmc_dens$y)])
# Assemble the results to a table
tibble(grid = bayes_est_grid, conjugate = bayes_est_conj, 
       Laplace = bayes_est_laplace, MCMC = bayes_est_mcmc)
