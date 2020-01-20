# Chapter 3 Sampling and Resampling Techniques
# Ref: Machine learning using R , Apress 2019
# 3.10.1 Law of Large Numbers: LLN ----------------------------------------

library(data.table)
n <- 100 # number of toss
p <- 0.6 # probability of getting a head

set.seed(917)
dt <- data.table(binomial = rbinom(n, 1, p), count_of_heads = 1, mean = 0)
head(dt)

ifelse(dt$binomial[1] == 1, dt[1, 2:3] <- 1, 0)
head(dt)

# run an experiment with large number of times and see how the average of
# heads approaches probabiity of heads converge to a value

for (i in 2:n){
  dt$count_of_heads[i] <- ifelse(dt$binomial[i] == 1, 
                    dt$count_of_heads[i] <- dt$count_of_heads[i-1] + 1,
                    dt$count_of_heads[i-1])
  dt$mean[i] <- dt$count_of_heads[i]/i
}

# plot and see how the average converges
plot(dt$mean, type = "l", main = "Simulation of average no. of Heads",
     xlab = "Size of sample", ylab = "Sample mean of no. of Heads",
     ylim = c(0, 1))
abline(h = p, col = "red")


# 3.10.2 Central Limit Theorem --------------------------------------------
r <- 5000 # number of samples
n <- 10000 # size of each sample

lambda <- 0.6
Exponential_Samples <- matrix(rexp(n*r, lambda), r)
all.sample.sums <- apply(Exponential_Samples, 1, sum)
all.sample.means <- apply(Exponential_Samples, 1, mean)
all.sample.vars <- apply(Exponential_Samples, 1, var)

# plot the sum, mean, and variance for each sample
par(mfrow=c(2,2))
hist(Exponential_Samples[1, ], col = "gray", main = "Distr. of One Sample")
hist(all.sample.sums, col = "gray", main = "Sampling Distr. the Sum")
hist(all.sample.means, col = "gray", main = "Sampling Distri. the mean")
hist(all.sample.vars, col = "gray", main = "Sampling Distr. the Variance")

shapiro.test(all.sample.means)

