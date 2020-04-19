# an example on how to use survey package in R
# http://users.stat.umn.edu/~gmeeden/classes/5201/handouts/survey_handout.pdf


# Generate data -----------------------------------------------------------

set.seed(506)
y <- c(rnorm(500), rnorm(500, mean=5))
x <- y + rnorm(1000)
group <- c(rep('a', 500), rep('b', 500))
pop <- data.frame(group, x, y)

library(ggplot2)
qplot(x, y, col=group)


# Simple random sampling --------------------------------------------------

srs.rows <- sample(1000, 100)
srs <- pop[srs.rows, ]
library(survey)
fpc.srs <- rep(1000, 100)
des.srs <- svydesign(id= ~1, strata = NULL, data = srs, fpc = fpc.srs)

# find the total, mean, and 95% CI
svytotal(~y, design = des.srs) # 2549.5
svymean(~y, design = des.srs)
confint(svytotal(~y, design = des.srs))

# using ratio estimtator
known.x.total <- sum(pop$x)
srs.ratio <- svyratio(numerator = ~y, denominator = ~x, design = des.srs)
srs.ratio

predict(srs.ratio, total = known.x.total) # y = 2506.511

# Stratified sampling -----------------------------------------------------
str.rows <- c(sample(500,50), sample(501:1000, 50))
str <- pop[str.rows, ]
fpc.str <- rep(500, 100)
des.str <- svydesign(id=~1, strata = ~group, data = str, fpc = fpc.str)

# find the total
svytotal(~y, des.str) # 2654.8

# True y in population as total -------------------------------------------

sum(pop$y) # sum(y) = 2531.618
