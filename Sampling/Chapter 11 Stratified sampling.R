# Chapter 11 Stratified sampling


# 11.9. COMPUTING NOTES ---------------------------------------------------

# Read in data

moosedat <- read.table(file="http://www.stat.sfu.ca/???thompson/data/moosedata")
head(moosedat)
# write.csv(moosedat, "moosedata.csv")

# rename for estimate
stratum <- moosedat$str
y <- moosedat$moose

# total size
length(y)

# stratum sample sizes
table(stratum)

# two ways to obtain stratum sample means and sample variances
# first, use tapply
tapply(y, stratum, mean)
tapply(y, stratum, var)

# second, subsetting then repeat for each
y1 <- y[stratum == 1]
y1
length(y1)
mean(y1)
var(y)

# simulation of population using sample data/bootstrapping
# N1 = 122, N2 = 57, N3 = 22, L = 3
# n1 = 39, n2 = 38, n3 = 21
y1 <- y[stratum == 1]
y2 <- y[stratum == 2]
y3 <- y[stratum == 3]
y1aug <- c(rep(y1, 3), sample(y1, 5))
y2aug <- c(y2, sample(y2, 19))
y3aug <- c(y3, sample(y3, 1))
tauhat <- numeric()

N1 = 122; N2 = 57; N3 = 22
n1 = 39; n2 = 38; n3 = 21

# repead 10,000 times
b <- 10000
for (k in 1:b){
  s1 <- sample(N1, n1)
  tauhat1 <- N1 * mean(y1aug[s1])
  
  s2 <- sample(N2, n2)
  tauhat2 <- N2 * mean(y2aug[s2])
  
  s3 <- sample(N3, n3)
  tauhat3 <- N3 * mean(y3aug[s3])
  
  tauhat[k] <- tauhat1 + tauhat2 + tauhat3
}

hist(tauhat)
mean(tauhat)
var(tauhat)

tau <- mean(y1aug) + sum(y2aug) + sum(y3aug)
tau
mean((tauhat - tau)^2)
sqrt(mean((tauhat - tau)^2))
