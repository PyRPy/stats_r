
# Lesson 25: Power of a Statistical Test ----------------------------------
# reference: https://stats.idre.ucla.edu/r/dae/power-analysis-for-one-sample-t-test/
# https://online.stat.psu.edu/stat415/lesson/25/25.3

library(pwr)
# Example 25-2
# Ho: mu = 100; Ha: mu > 100

pwr.t.test(d=(108-100)/16,
           n = 16,
           sig.level=0.05,
           type="one.sample",
           alternative="greater")
# results are not same as those in the lecture
pwr.t.test(
           n = 16,
           sig.level=0.05,
           power = 0.6406,
           type="one.sample",
           alternative="greater")

xhat = 100 + 0.525*16

# Example 25-3
# Ho: mu = 100; Ha: mu > 100

pwr.t.test(d=(108-100)/16,
           n = 64,
           sig.level=0.05,
           type="one.sample",
           alternative="greater")
# mu = 112
pwr.t.test(d=(112-100)/16,
           n = 64,
           sig.level=0.05,
           type="one.sample",
           alternative="greater")

# mu = 116, power = 1
pwr.t.test(d=(116-100)/16,
           n = 64,
           sig.level=0.05,
           type="one.sample",
           alternative="greater")
