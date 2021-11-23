# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/wilcoxon-signed-rank-test

library(MASS)         
head(immer) 

boxplot(immer$Y1, immer$Y2)

wilcox.test(immer$Y1, immer$Y2, paired=TRUE) 

# Welch t test
t.test(immer$Y1, immer$Y2)

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/wilcox.test.html
require(graphics)

## Formula interface.
boxplot(Ozone ~ Month, data = airquality)
wilcox.test(Ozone ~ Month, data = airquality,
            subset = Month %in% c(5, 8))

## accuracy in ties determination via 'digits.rank':
wilcox.test( 4:2,      3:1,     paired=TRUE) # Warning:  cannot compute exact p-value with ties
wilcox.test((4:2)/10, (3:1)/10, paired=TRUE) # no ties => *no* warning
wilcox.test((4:2)/10, (3:1)/10, paired=TRUE, digits.rank = 9) # same ties as (4:2, 3:1)

# https://www.geeksforgeeks.org/wilcoxon-signed-rank-test-in-r-programming/
# R program to illustrate
# one-sample Wilcoxon signed-rank test

# The data set
set.seed(1234)
myData = data.frame(
  name = paste0(rep("R_", 10), 1:10),
  weight = round(rnorm(10, 30, 2), 1)
)

# Print the data
print(myData)

# One-sample wilcoxon test
result = wilcox.test(myData$weight, mu = 25)

# Printing the results
print(result)

# R program to illustrate
# Paired Samples Wilcoxon Test

# The data set
# Weight of the rabbit before treatment
before <-c(190.1, 190.9, 172.7, 213, 231.4,
           196.9, 172.2, 285.5, 225.2, 113.7)

# Weight of the rabbit after treatment
after <-c(392.9, 313.2, 345.1, 393, 434,
          227.9, 422, 383.9, 392.3, 352.2)

# Create a data frame, pay attention to the sequence or order of the pair
myData <- data.frame(
  group = rep(c("before", "after"), each = 10),
  weight = c(before, after)
)

# Print all data
print(myData)

# Paired Samples Wilcoxon Test
result = wilcox.test(before, after, paired = TRUE)

# Printing the results
print(result)


