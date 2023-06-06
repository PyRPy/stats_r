
# Estimating a Binomial Success Parameter ---------------------------------
# Section 8.4.1

library(R2OpenBUGS)

model_binom <- function() {
    # prior
    y ~ dbin(pi, n)
    #posterior
    pi ~ dbeta(alpha, beta)
}

# data as input
mydata <- list(y = 7, n = 50, alpha = 0.5, beta = 0.5)


# run three Markov Chains
inits0 <- function() {
  list(pi = 0.1, pi = 0.5, pi = 0.9)
}

# run the model from R environment
model_out <- bugs(data=mydata,
                inits = inits0,
                parameters.to.save = c("pi"),
                model.file = model_binom,
                n.chains = 3,
                n.iter = 10000,
                debug = TRUE)

model_out$summary

#               mean         sd       2.5%      25%    50%     75%     97.5%     Rhat n.eff
# pi       0.1466407 0.04845567 0.06541975 0.112275 0.1421 0.17680 0.2523025 1.001021 15000
# deviance 4.6176065 1.37315094 3.65800000 3.750000 4.0825 4.93925 8.5451749 1.000941 15000
