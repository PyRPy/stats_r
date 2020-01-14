# template for intro to survey package
# http://users.stat.umn.edu/~gmeeden/classes/5201/handouts/survey_handout.pdf
library(ggplot2)
library(survey)
# intro to survey package -------------------------------------------------

set.seed(1)
y <- c(rnorm(500), rnorm(500, mean = 5))
x <- y + rnorm(1000)
group <- c(rep('a', 500), rep('b', 500))
pop <- data.frame(group, x, y)
head(pop)

ggplot(pop, aes(x= x, y = y, col=group)) +
  geom_point()

# data preparation
srs.rows <- sample(1000, 100)
srs <- pop[srs.rows, ] # no strata

str.rows <- c(sample(500, 50), sample(501:1000, 50))
str <- pop[str.rows, ] # with strata

# simple random sample without stratification -----------------------------
fpc.srs <- rep(1000, 100)
des.srs <- svydesign(id = ~1, strata = NULL, data = srs, fpc = fpc.srs)

svytotal(~y, design = des.srs)
svymean(~y, design = des.srs)
confint(svytotal(~y, design = des.srs))

# predictions
known.x.total <- sum(pop$x)
(srs.ratio <- svyratio(numerator = ~y, denominator = ~ x, design = des.srs))
predict(srs.ratio, total = known.x.total)

sum(pop$y)


# stratified random sampling ----------------------------------------------

fpc.str <- rep(500, 100)
des.str <- svydesign(id = ~1, strata = ~ group, data = str, fpc = fpc.str)
svytotal(~y, des.str)

# prediction
predict(svyratio(numerator = ~y, denominator = ~ x, design = des.str),
        total = known.x.total)
