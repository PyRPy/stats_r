
# 2 k factorial design ----------------------------------------------------
# use the examples in the following tutorial as template
# https://towardsdatascience.com/design-of-experiments-with-r-e54167fac490
inches_burned <- c(41.0, 30.5, 47.5, 27.0,
                   39.5, 26.5, 48.0, 27.5)

library(SixSigma)
# Design the experiment (2^3)
ExperimentDesign <- expand.grid(A = gl(2, 1, labels = c("-", "+")),
                                B = gl(2, 1, labels = c("-", "+")),
                                C = gl(2, 1, labels = c("-", "+")))

ExperimentDesign

ExperimentDesign$ord <- sample(1:8, 8)
ExperimentDesign[order(ExperimentDesign$ord), ]

# Create replicates
ss.data.doe1 <- data.frame(repl = rep(1:2, each = 8),
                           rbind(ExperimentDesign))
ss.data.doe1

ss.data.doe1$response <- c(5.33, 6.99, 4.23, 6.61,
                           2.26, 5.75, 3.26, 6.24,
                           5.7, 7.71, 5.13, 6.76,
                           2.79, 4.57, 2.48, 6.18)
ss.data.doe1

# Get the average score for each experiment design
aggregate(response ~ A + B + C,
          FUN = mean, data = ss.data.doe1)

# Get restuls
doe.model <- lm(response ~ A + B + C +
                  A * B + A * C + B * C +
                  A * B * C,
                data = ss.data.doe1)

summary(doe.model)

