# Chapter 12 Cluster and systematic sampling
# a survy of sea otters in a coastal study region
# some calculaitons for cluster
# sampling example - sea otter survey

# n = 4, `N = 16, M = 33
ys <- c(3, 24, 7, 2)
Mi <- c(1, 4, 3, 2)
N <- 16 # primary units
M <- 33 # secondary units - total
n <- 4  # 4 random samples
plot(Mi, ys, main = "Second Units vs Otters Counts")

# Expansion estimator -----------------------------------------------------
# unbiased if design was SRS
Tauhat <- N * mean(ys)
Tauhat
ssq <- var(ys)
ssq

varhatTauhat <- N^2 * (1-n/N) * ssq / n
varhatTauhat

seTauhat <- sqrt(varhatTauhat)
seTauhat
qt(0.9, 3)
Tauhat + qt(0.9, 3) * seTauhat
Tauhat - qt(0.9, 3) * seTauhat
Tauhat + qt(c(0.1, 0.9), 3) * seTauhat # very large range

# Hansen Hurwitz estimator
# unbiased if design had been PPS
pi <- Mi/M
pi
Tauhatp <- mean(ys/pi)
Tauhatp

varhatTauhatp <- var(ys/pi) / n
varhatTauhatp

ys/pi
var(ys/pi)

seTauhatp <- sqrt(varhatTauhatp)
seTauhatp
round((Tauhatp + qt(c(0.1, 0.9), n-1) * seTauhatp), 1)

# Horvitz Thompson estimator
pii <- 1 - (1-pi)^n
pii
sum(ys/pii)
ys/pii


# Systematic sample - rainfall example ------------------------------------

# rainfall survey: sample rainfall at a site every 10th day over 365 days
# M = 365
# k = 10
M <- 365
k <- 10
set.seed(506)
y <- abs(rnorm(365, 2, 1))
hist(y)
start <- sample(1:k, 1)
s <- seq(start, M, k)
s

# the ratio estimate of mean daily rainfall during the year:
mean(y[s])

# the unbiased estimator of mean daily rainfall
10 * sum(y[s]) / 365

