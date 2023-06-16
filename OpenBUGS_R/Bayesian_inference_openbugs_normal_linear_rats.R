
# Simple linear regression using BUGS -------------------------------------
# 10.5 Hierarchical Normal Linear Models


library(R2OpenBUGS)

# model
hmn_model <- function() {
  for (i in 1:N) {
    for (j in 1:T) {
      Y[i,j] ~ dnorm(mu[i,j], tau.c)
      mu[i,j] <- alpha[i] + beta[i] * (x[j] - xbar)
    }
    alpha[i] ~ dnorm(alpha.c, tau.alpha)
    beta[i] ~ dnorm(beta.c, tau.beta) # reversed tau.beta, dangerous!
  }
  tau.c ~ dgamma(0.001, 0.001)
  sigma <- 1 / sqrt(tau.c)
  alpha.c ~ dnorm(0.0, 1.0E-6)

  # prior of random effects variance
  # uniform on SD
  sigma.alpha ~ dunif(0, 100)
  sigma.beta ~ dunif(0, 100)
  tau.alpha <- 1 / (sigma.alpha * sigma.alpha)
  tau.beta <- 1 / (sigma.beta * sigma.beta)

  beta.c ~ dnorm(0.0, 1.0E-6)
  alpha0 <- alpha.c - xbar * beta.c


}


# data
data_rats <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0),
                  xbar = 22,
                  N = 30,
                  T = 5,
                  Y = structure(
                    .Data = c(151, 199, 246, 283, 320,
                              145, 199, 249, 293, 354,
                              147, 214, 263, 312, 328,
                              155, 200, 237, 272, 297,
                              135, 188, 230, 280, 323,
                              159, 210, 252, 298, 331,
                              141, 189, 231, 275, 305,
                              159, 201, 248, 297, 338,
                              177, 236, 285, 350, 376,
                              134, 182, 220, 260, 296,
                              160, 208, 261, 313, 352,
                              143, 188, 220, 273, 314,
                              154, 200, 244, 289, 325,
                              171, 221, 270, 326, 358,
                              163, 216, 242, 281, 312,
                              160, 207, 248, 288, 324,
                              142, 187, 234, 280, 316,
                              156, 203, 243, 283, 317,
                              157, 212, 259, 307, 336,
                              152, 203, 246, 286, 321,
                              154, 205, 253, 298, 334,
                              139, 190, 225, 267, 302,
                              146, 191, 229, 272, 302,
                              157, 211, 250, 285, 323,
                              132, 185, 237, 286, 331,
                              160, 207, 257, 303, 345,
                              169, 216, 261, 295, 333,
                              157, 205, 248, 289, 316,
                              137, 180, 219, 258, 291,
                              153, 200, 244, 286, 324),
                              .Dim = c(30,5)))

# initials
inits_rats <- function() {
  list(alpha = c(250, 250, 250, 250, 250, 250, 250, 250, 250,
                 250, 250, 250, 250, 250, 250, 250, 250, 250,
                 250, 250, 250, 250, 250, 250, 250, 250, 250,
                 250, 250, 250),
       beta = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
       alpha.c = 150,
       beta.c = 10,
       tau.c = 1,
       sigma.alpha = 1,
       sigma.beta = 1)
}

# run BUGS model for sampling
model_output <- bugs(data=data_rats,
                inits = inits_rats,
                parameters.to.save = c("alpha0", "beta.c", "sigma"),
                model.file = hmn_model,
                n.chains = 1,
                n.iter = 20000,
                debug = TRUE)

# print out result
print(round(model_output$summary, 4))
print(model_output)

# results are very different than the table in the manual examples vol. 1
#               mean      sd      2.5%       25%       50%       75%     97.5%
# alpha0    244.7424 12.4942  219.8000  236.6000  244.9000  253.2000  268.8025
# beta.c     -0.1002  0.1150   -0.3225   -0.1778   -0.1063   -0.0239    0.1251
# sigma      13.7007  0.9125   12.0600   13.0700   13.6500   14.2800   15.6400
# deviance 1210.7093  9.8163 1193.0000 1204.0000 1210.0000 1217.0000 1232.0000
