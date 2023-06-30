
# Seeds: Random effect logistic regression --------------------------------
# https://www.multibugs.org/examples/latest/Seeds.html


library(R2OpenBUGS)

model_logit <- function() {
  for (i in 1:N) {
    r[i] ~ dbin(p[i], n[i])
    beta[i] ~ dnorm(0.0, tau)
    logit(p[i]) <- alpha0 + alpha1 * x1[i] + alpha2 * x2[i] +
      alpha12 * x1[i] * x2[i] + beta[i]
  }
  alpha0 ~ dnorm(0.0, 1.0E-6)
  alpha1 ~ dnorm(0.0,1.0E-6)
  alpha2 ~ dnorm(0.0,1.0E-6)
  alpha12 ~ dnorm(0.0,1.0E-6)
  sigma ~ dunif(0, 10)
  tau <- 1 / pow(sigma, 2)
}

# data as input
mydata <- list(r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3),
                n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
                x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                N = 21)

# run three Markov Chains
inits0 <- function() {
  list(alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha12 = 0, sigma = 8)
  list(alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha12 = 0, sigma = 1,
    beta = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
}

# run the model from R environment
model_out <- bugs(data=mydata,
                inits = inits0,
                parameters.to.save = c("alpha0", "alpha1", "alpha2", "alpha12", "sigma"),
                model.file = model_logit,
                n.chains = 2,
                n.iter = 10000,
                debug = TRUE)

round(model_out$summary, 4)
#              mean     sd    2.5%     25%     50%      75%    97.5%   Rhat n.eff
# alpha0    -0.5499 0.2146 -0.9898 -0.6820 -0.5494  -0.4165  -0.1183 1.0017  1900
# alpha1     0.0659 0.3369 -0.6256 -0.1472  0.0738   0.2815   0.7251 1.0012  4400
# alpha2     1.3554 0.3041  0.7441  1.1630  1.3530   1.5500   1.9720 1.0021  1300
# alpha12   -0.8332 0.4751 -1.7860 -1.1350 -0.8294  -0.5248   0.0888 1.0012  5200
# sigma      0.3506 0.1448  0.1057  0.2508  0.3344   0.4327   0.6798 1.0071   830
# deviance 100.0842 6.2654 89.2400 95.5800 99.4900 104.2000 113.2000 1.0012  5000



# Use GLM model -----------------------------------------------------------
# https://math.unm.edu/~luyan/stat57918/week5.pdf
data_seeds <- data.frame(
  r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3),
  n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
  x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
)

data_seeds$Y <- cbind(data_seeds$r, data_seeds$n - data_seeds$r)
model_logit2 <- glm(Y ~ x1 + x2 + x1:x2,
                    family=binomial(link = "logit"),
                    data = data_seeds)
summary(model_logit2)

#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -0.5582     0.1260  -4.429 9.46e-06 ***
# x1            0.1459     0.2232   0.654   0.5132
# x2            1.3182     0.1775   7.428 1.10e-13 ***
# x1:x2        -0.7781     0.3064  -2.539   0.0111 *
