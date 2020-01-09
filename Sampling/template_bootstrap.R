# Bootstrap in R ----------------------------------------------------------
# ref: https://www.datacamp.com/community/tutorials/bootstrap-r
head(iris)
library(boot)

myfunc <- function(data, indices, cor.type){
  dt <- data[indices, ]
  c(cor(dt[, 1], dt[, 2], method = cor.type),
    median(dt[, 1]),
    median(dt[, 2])
    )
}

set.seed(12345)
myBootstrap <- boot(iris, myfunc, R = 1000, cor.type = "s")

head(myBootstrap$t)
median(iris[, 1]) # 5.8

head(myBootstrap$t0)

# plot to show CI
plot(myBootstrap, index = 1) # cor
plot(myBootstrap, index = 2) # too few points
plot(myBootstrap, index = 3) # too few points

# get the CF for correlation coefficients
boot.ci(myBootstrap, index = 1) # studentized interval

boot.ci(myBootstrap, index = 1, type = 'norm')

boot.ci(myBootstrap, index = 1, type = "basic")$basic

plot(myBootstrap, index = 3)

table(myBootstrap$t[, 3])


# Replication of results --------------------------------------------------

tableOfIndices <- boot.array(myBootstrap, indices = T)
tableOfIndices[1, ]

tableOfAppearances <- boot.array(myBootstrap)
tableOfAppearances[1, ]

onceAgain <- apply(tableOfIndices, 1, myfunc, data=iris, cor.type='s')
head(t(onceAgain))
head(myBootstrap$t)

all(t(onceAgain) == myBootstrap$t)
