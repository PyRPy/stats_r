# 1.3 exercises

# 1.1 How would you check whether two vectors are the same if theymay contain 
# missing (NA) values? (Use of theidenticalfunction isconsidered cheating!)
# E1.1 
x <- c(7, 9, NA, NA, 13)
y <- c(7, 9, NA, NA, 13)

all(is.na(x) == is.na(y)) && all((x==y)[!is.na(x)])

z <- c(7, NA, NA, 9, 13)
all(is.na(x) == is.na(z)) && all((x==z)[!is.na(x)])

# 1.2 Ifxis a factor withnlevels andyis a lengthnvector, what happensif you 
# computey[x]?
# E1.2
x <- factor(c(1, 2, 3, 5, 3, 2, 5), levels = c(1, 2, 3, 4, 5))
y <- c(2, 4, 6, 8, 10)
y[x]
x

# version 2
x <- factor(c("Huey", "Dewey", "Louie", "Huey"))
y <- c("blue", "red", "green")
y[x]
x
as.numeric(x) # [1] 2 1 3 2

# 1.3 Write the logical expression to use to extract girls between 7 and 14years 
# of age in the juul data set.
library(ISwR)
head(juul)
str(juul)
juul[juul$sex == 2, ]

# method 1
juul[juul$sex == 2 && juul$age >= 7 && juul$age <=14, ]

# method 2
subset(juul, age >=7 & age <=14 & sex == 2)

# 1.4 What happens if you change the levels of a factor (with levels) and give the 
# same value to two or more levels?
# answer in book --- the levels with the same name are collapsed into one.

# 1.5 On p. 27,replicate was used to simulate the distribution of the mean of 20 
# random numbers from the exponential distribution by re-peating the operation 
# 10 times. How would you do the same thing with sapply?

# page - 27
set.seed(123)
replicate(10, mean(rexp(20)))

apply(matrix(replicate(10, rexp(20)), nrow=20, ncol=10), 2, mean)

# sapply(replicate(10, rexp(20)), mean)     

# solution in book
sapply(1:10, function(i) mean(rexp(20)))


