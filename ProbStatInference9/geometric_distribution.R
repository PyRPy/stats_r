# simulate a geometric distribution
library(ggplot2)
# to find the first instance of heads
replications <- replicate(100000, which(rbinom(100, 1, 0.2) == 1)[1])

# histrogram for distribution
hist(replications)
qplot(replications)
