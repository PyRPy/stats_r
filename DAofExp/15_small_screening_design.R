# chapter 15
# small screening designs
# page 548 - fraction factorial experiments
ssd <- data.frame(A = c(-1, -1, 1, -1, 1, 1),
                   B = c(-1, 1, -1, 1, -1, 1),
                   C = c(-1, -1, 1, 1, 1, -1),
                   D = c( 1, -1, -1, -1, 1, 1),
                   E = c( 1, -1, -1, 1, -1, 1),
                   F = c( 1, 1, -1, -1, 1, -1),
                   G = c(-1, 1, 1, -1, -1, 1),
                   H = c( 1, 1, 1, -1, -1, -1),
                   I = c( 1, -1, 1, 1, -1, -1),
                   J = c(-1, 1, -1, 1, 1, -1),
                   y = c(40.83, 28.17, 13.98, 32.85, 39.78, 52.08))

library(leaps)

regsubsets.outssd <- regsubsets(y ~ A+B+C+D+E+
                                   F + G + H + I + J, data = ssd, nbest = 3,
                                 nvmax = 3, method = "exhaustive")

summary.outssd <- summary(regsubsets.outssd)
as.data.frame(summary.outssd$outmat)
round(summary.outssd$rsq, 4)

alias(aov(y ~ A + B + C + D + E + F + G + H + I + J ,ssd))

