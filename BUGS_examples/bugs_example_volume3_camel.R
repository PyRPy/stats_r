
# Camel: Multivariate normal with structured missing data -----------------

# https://www.multibugs.org/examples/latest/Camel.html

library(R2OpenBUGS)

model.camel <- function() {
  for (i in 1 : N){
    Y[i, 1 : 2] ~ dmnorm(mu[], tau[ , ])
  }
  mu[1] <- 0
  mu[2] <- 0
  tau[1 : 2,1 : 2] ~ dwish(R[ , ], 2)
  R[1, 1] <- 0.001
  R[1, 2] <- 0
  R[2, 1] <- 0;
  R[2, 2] <- 0.001
  Sigma2[1 : 2,1 : 2] <- inverse(tau[ , ])
  rho <- Sigma2[1, 2] / sqrt(Sigma2[1, 1] * Sigma2[2, 2])
}

data <- list(N= 12,
             Y = structure(
               .Data = c(1,1,
                         1,-1,
                         -1,1,
                         -1, -1,
                         2, NA,
                         2, NA,
                         -2, NA,
                         -2, NA,
                         NA, 2,
                         NA, 2,
                         NA, -2,
                         NA, -2),
               .Dim = c(12, 2)))

inits <- function() {
  list(tau = structure(.Data = c(0.1,0,0,0.1), .Dim = c(2,2)),
       Y = structure(
         .Data = c(NA,NA,
                   NA,NA,
                   NA,NA,
                   NA, NA,
                   NA, 1,
                   NA, 1,
                   NA, 1,
                   NA, 1,
                   1, NA,
                   1, NA,
                   1, NA,
                   1, NA),
         .Dim = c(12, 2)))

  list(tau = structure(.Data = c(0.5,0,0,0.5), .Dim = c(2,2)),
       Y = structure(
         .Data = c(NA,NA,
                   NA,NA,
                   NA,NA,
                   NA, NA,
                   NA, 2,
                   NA, 2,
                   NA, 2,
                   NA, 2,
                   3, NA,
                   3, NA,
                   3, NA,
                   3, NA),
         .Dim = c(12, 2)))
}

parameters <- c("Sigma2", "rho", "tau")

fit.camel <- bugs (data, inits,
                     parameters,
                     model.camel,
                     n.chains=2,
                     DIC = FALSE,
                     n.iter=2000,
                     debug=TRUE )
plot(fit.camel)
print(fit.camel)
#             mean  sd 2.5%  25%  50%  75% 97.5% Rhat n.eff
# Sigma2[1,1]  1.7 0.9  0.7  1.2  1.5  2.1   3.9    1  2000
# Sigma2[1,2] -1.6 1.6 -5.3 -2.4 -1.4 -0.7   1.2    1  2000
# Sigma2[2,1] -1.6 1.6 -5.3 -2.4 -1.4 -0.7   1.2    1  2000
# Sigma2[2,2]  5.4 3.9  1.6  2.9  4.2  6.5  16.0    1  2000
# rho         -0.5 0.4 -0.9 -0.8 -0.6 -0.3   0.4    1   500
# tau[1,1]     1.4 0.9  0.4  0.8  1.2  1.9   3.9    1   110
# tau[1,2]     0.5 0.5 -0.2  0.2  0.4  0.8   1.6    1   110
# tau[2,1]     0.5 0.5 -0.2  0.2  0.4  0.8   1.6    1   110
# tau[2,2]     0.5 0.3  0.1  0.3  0.4  0.6   1.2    1   140
