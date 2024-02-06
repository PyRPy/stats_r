
# Other generalized linear models -----------------------------------------

library("rstanarm")
library("MASS")

# set the random seed
SEED <- 3579

# Simulate fake data
n <- 100
x <- runif(n, -2, 2)
a <- 1
b <- 2
linpred <- a + b*x
y <- rpois(n, exp(linpred))
fake <- data.frame(x=x, y=y)
head(fake)

# Fit Poisson regression model
fit_fake <- stan_glm(y ~ x, family=poisson(link="log"), data=fake, refresh=0)
print(fit_fake)

plot(x, y)
curve(exp(coef(fit_fake)[1] + coef(fit_fake)[2]*x), add=TRUE)

# Overdispersion
# pay attention to how to use list in the program as 'container'
phi_grid <- c(0.1, 1, 10)
K <- length(phi_grid)
y_nb <- as.list(rep(NA, K))
fake_nb <- as.list(rep(NA, K))
fit_nb <- as.list(rep(NA, K))
for (k in 1:K){
  y_nb[[k]] <- rnegbin(n, exp(linpred), phi_grid[k])
  fake_nb[[k]] <- data.frame(x=x, y=y_nb[[k]])
  fit_nb[[k]] <- stan_glm(y ~ x, family=neg_binomial_2(link="log"), data=fake_nb[[k]], refresh=0)
  print(fit_nb[[k]])
}

par(mfrow=c(1, 3))
for (k in 1:K) {
  plot(x, y_nb[[k]])
  curve(exp(coef(fit_nb[[k]])[1] + coef(fit_nb[[k]])[2]*x), add=TRUE)
}
par(mfrow=c(1, 1))


# 15.3 Logistic-binomial model --------------------------------------------
# We demonstrate with a simple model of basketball shooting. We first
# simulate N = 100 players each shooting n = 20 shots, where the
# probability of a successful shot is a linear function of
# height(30% for a 5'9" player, 40% for a 6' tall player, and so forth):
N <- 100
height <- rnorm(N, 72, 3)
p <- 0.4 + 0.1 * (height - 72) / 3
n <- rep(20, N)
y <- rbinom(N, n, p)
data <- data.frame(n=n, y=y, height=height)

# n fit and display a logistic regression predicting success
# probability given height
fit_1a <- stan_glm(cbind(y, n-y) ~ height, family = binomial(link = "logit"),
                   data = data)
print(fit_1a)
#             Median MAD_SD
# (Intercept) -10.4    1.2
# height        0.1    0.0

