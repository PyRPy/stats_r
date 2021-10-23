# chi-squared goodness of fit test testing whether coin flips differ 
# significantly from what we expect
# https://researchguides.library.vanderbilt.edu/c.php?g=156859&p=3057842
observedFlips = c(46, 41)        # actual absolute: (heads, tails)
expectedProb = c(0.5, 0.5)       # expected relative: (heads, tails)

chisq.test(
  x = observedFlips,
  p = expectedProb, 
)

#  p-value = 0.5919
#  d.f = 2 - 1 = 1
# X-squared = 0.28736