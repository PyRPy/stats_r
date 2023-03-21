
# Hypothesis test on data from exponential distribution  ------------------
# Ho : data comes from exponential distribution with lambda = 20
# Ha : data does not come from an exponential distribution with lambda = 20

## input data
library(dplyr)
dat <- read.table("Ex6_1-5.txt", quote="\"", comment.char="")

## organize and transform the data
dat %>% mutate(new_bin = cut(V1, breaks = c(0,9,18,27,36,45,54,63,72, Inf)))

dat_table <-  dat %>% mutate(new_bin = cut(V1, breaks = c(0,9,18,27,36,45,54,63,72, Inf)))

table(dat_table$new_bin)

pexp(9, rate = 1/20)

intervals = c(0,9,18,27,36,45,54,63,72, Inf)

## construct table for chi-square test
pexp(intervals, rate = 1/20)
Class = paste0("A", seq(1:9))
Probability = diff(pexp(intervals, rate = 1/20))
Frequency = as.numeric(table(dat_table$new_bin))
Expected = nrow(dat) * Probability

## prepare the statistics
q8 = sum((Frequency - Expected)^2 / Expected)

1-pchisq(q8,df=9-1)

# another way of conducting test using chisq.test() function directly

chisq.test(Frequency, p=Probability) # same p-value as 'manually' conducted test

# verify sum of Probability = 1
sum(Probability)

## draw conclusion on the hypothesis test
## since p-value from the hypothesis test p-value = 0.7912 > 0.05,
## we can conclude that there is no sufficient evidence to reject
## the null hypothesis, therefore, the data indeed comes from a exponential
## distribution with lambda = 20
