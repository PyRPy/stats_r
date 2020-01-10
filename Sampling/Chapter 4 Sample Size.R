# Chapter 4 Sample Size

# calculate required sample size ------------------------------------------
library(pwr)
delta <- 20 # different to detect
sigma <- 60 # variance
d <- delta/sigma # effective size
pwr.t.test(d = d,
           sig.level = 0.05,
           power = 0.9,
           type = "two.sample")

# https://www.r-bloggers.com/calculating-required-sample-size-in-r-and-sas/

# Example 1 Size required to estimate the caribou polulation total with d
delta <- 2000
sigma <- 286*sqrt(919)
d <- delta/sigma
pwr.t.test(d = d,
           sig.level = 0.10,
           power = 0.8,
           type = "one.sample")

# https://stats.idre.ucla.edu/r/dae/power-analysis-for-one-sample-t-test/

# http://www.r-tutor.com/elementary-statistics/interval-estimation/sampling-size-population-mean
zstar = qnorm(0.95)
sigma = 8670.1
E = 2000
zstar^2 * sigma^2 / E^2
sqrt(919)*286

# https://www.r-bloggers.com/n-is-for-n-sample-size-estimation-power-analysis-in-r/
library(pwr)
caffeine <- read.delim("caffeine_study.txt", header = TRUE)
head(caffeine)

caffeine$group <- factor(caffeine$group, labels = c("Control", "Experimental"))
library(effsize)

# effective size
cohen.d(caffeine$score, caffeine$group, pooled = TRUE, conf.level = 0.95)

# sample size
pwr.t.test(d = 0.624, sig.level = 0.05, power = 0.8, type = "two.sample",
           alternative = "two.sided")


pwr.t.test(d = 0.624, sig.level = 0.05, power = 0.8, type = "two.sample",
           alternative = "greater")

# comparing a range of school pass rates (50-79%) to a population value 
# (80%)
pls <- seq(0.5, 0.79, 0.01)
h <- ES.h(p1 = pls, p2 = 0.80)
nh <- length(h)

# two power levels
p <- c(0.8, 0.9)
np <- length(p)

# generating target sample sizes
sizes <- array(numeric(nh*np), dim = c(nh, np))
for (i in 1:np){
  for (j in 1:nh){
    pow_an <- pwr.p.test(n = NULL,
                         h = h[j],
                         sig.level = 0.05,
                         power = p[i], 
                         alternative = "less")
    sizes[j, i] <- ceiling(pow_an$n)
  }
}

# construct a dataframe
samp <- data.frame(cbind(pls, sizes))
colnames(samp) <- c("Pass_Rate", "Power.8", "Power.9")
summary(samp)

# plot the results
library(ggplot2)
passrate <- ggplot(samp, aes(x = pls, y = Power.8)) +
  geom_point() +
  geom_line()

passrate + 
  scale_y_continuous(breaks = seq(0, 11000, 1000)) +
  scale_x_continuous(labels = scales::percent)+
  labs(title = "Comparing pass rate below 80%",
       x = "Pass rate",
       y = "N needed to detect significant difference") +
  theme(plot.title = element_text(hjust = 0.5))

# two power levels
head(samp)
bothpowers<-ggplot(samp, aes(Pass_Rate)) +
  geom_line(aes(y=Power.8, colour="0.8")) +
  geom_line(aes(y=Power.9, colour="0.9")) +
  scale_y_continuous(breaks=seq(0,14000,1000)) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Comparing Pass Rates Below 80%",
       x = "Pass Rate", y = "N Needed to Detect Significant Difference", colour="Power") +
  theme(plot.title=element_text(hjust=0.5))
bothpowers
