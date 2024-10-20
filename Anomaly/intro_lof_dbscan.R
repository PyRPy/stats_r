# lof method
library(dbscan)
head(mtcars)

plot(mtcars$hp, mtcars$mpg)
cars_mpg <- mtcars[, c("hp", "mpg")]
cars_lof <- lof(scale(cars_mpg), minPts = 7)
cars_mpg$score_lof <- cars_lof
plot(mpg ~ hp, data = cars_pmg, cex = score_lof, pch = 20)
which.max(cars_lof)
plot(cars_lof) # not a good example, no outliters?
