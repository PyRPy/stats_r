
# Lesson 25: Power of a Statistical Test ----------------------------------
# reference: https://stats.idre.ucla.edu/r/dae/power-analysis-for-one-sample-t-test/
# https://online.stat.psu.edu/stat415/lesson/25/25.3

library(pwr)
# Example 25-2
# Ho: mu = 100; Ha: mu > 100

pwr.norm.test(d=(108-100)/16,
           n = 16,
           sig.level=0.05,
           alternative="greater")
# results are not same as those in the lecture
pwr.norm.test(d=(112-100)/16,
           n = 16,
           sig.level=0.05,
           alternative="greater")

pwr.norm.test(d=(116-100)/16,
              n = 16,
              sig.level=0.05,
              alternative="greater")

# change a = 0.01
pwr.norm.test(d=(108-100)/16,
              n = 16,
              sig.level=0.01,
              alternative="greater")
# power = 0.3720806, getting close; changed from t to norm test

# Example 25-3
# Ho: mu = 100; Ha: mu > 100
## changed from pwr.t.test to pwr.norm.test, results are close now.##
pwr.norm.test(d=(108-100)/16,
           n = 64,
           sig.level=0.05,
           alternative="greater")
# mu = 112
pwr.norm.test(d=(112-100)/16,
           n = 64,
           sig.level=0.05,
           alternative="greater")

# mu = 116, power = 1
pwr.t.test(d=(116-100)/16,
           n = 64,
           sig.level=0.05,
           type="one.sample",
           alternative="greater")

# Example 8.5-2 version 9
# Ho : mu = 60; Ha: mu > 60
pwr.norm.test(d=(62-60)/10,
           n = 25,
           sig.level=0.05,
           alternative="greater")

# Example 25-5 proportion test
# https://online.stat.psu.edu/stat415/lesson/25/25.3
h = ES.h(0.55, 0.5)
pwr.p.test(h=h,
           power = 0.8,
           sig.level=0.01,
           alternative="greater")
# sample size = 1001, matched !
