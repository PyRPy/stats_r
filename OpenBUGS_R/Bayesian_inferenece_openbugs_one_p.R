
# one proportion sampling and inference using openbugs --------------------
# https://www.r-tutor.com/bayesian-statistics/openbugs

# problem is to find the proportion (percentage) of smokers in a survey

library(R2OpenBUGS)

model <- function() {
  # prior
  p ~ dbeta(1, 1)

  # likelihood
  y ~ dbin(p, N)
}

model.file <- file.path(tempdir(), "model.txt")
write.model(model, model.file)

# load the data
library(MASS)
tbl <- table(survey$Smoke)
N <- as.numeric(sum(tbl))
N

y <- N - as.numeric(tbl["Never"])
y

# put data into a list
data <- list("N", "y")
params <- c("p")

# set initial parameters
inits <- function() { list(p=0.5)}

# run OpenBUGS
out <- bugs(data,
            inits,
            params,
            model.file,
            n.iter = 10000)

# check results or outputs
all(out$summary[, "Rhat"] < 1.1)

# find posterior mean
out$mean["p"]  # 0.2014755


out2 <- bugs(data,
            inits,
            params,
            model.file,
            codaPkg = TRUE,
            n.iter = 10000)

out3 <- read.bugs(out2)

# find point estimate and 95% credible interval
out4 <- summary(out3, q = c(0.025, 0.975))

out4$stat["p",]
out4$q["p", ]


# more diagnosis
gelman.plot(out3)
