# simulate sum of two tetrhedral dice

sum <- c()
set.seed(415)
for (i in 1:1000){
  dice1 <- sample(1:4, 1)
  dice2 <- sample(1:4, 1)
  sum[i] <- dice1 + dice2
}

hist(sum)



