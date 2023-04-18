
# two proportions test ----------------------------------------------------
# is the dice a fair one? what's the chance to get 6
# Ho: p = 1/6
# Ha: p > 1/6

# total observations: 8000
# number 6 shows up 1389 times
p = 1389 / 8000
p > 1/6

n = 8000

# two proportion test
prop.test(c(1389, 1), c(8000, 6), alternative = "greater")
# 95 percent confidence interval:
# -0.2503534  1.0000000

# using exact binomial test
binom.test(c(1389, 8000-1389), 1/6, alternative = "greater")
# 95 percent confidence interval:
# 0.1666852 1.0000000
