# chapter 02 Getting started with ggplot2

library(ggplot2)
library(dplyr)
library(broom)

str(mpg)

# key components
ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()

ggplot(mpg, aes(displ, cty, colour = class)) +
  geom_point()

# colors
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = "blue")) # color scaled, legend added
ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "blue") # point given color blue

# facetting
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~ ~class)

# plot geoms
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.2) # smaller, wiggly more

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 1) # method default = 'loess'

library(mgcv)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x)) # for large scale data, n > 1000

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "lm")

library(MASS)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "rlm") # resistant to 'outliers'

# 2.6.2
# Boxplots and Jittered Points
ggplot(mpg, aes(drv, hwy)) +
  geom_point()

ggplot(mpg, aes(drv, hwy)) + geom_jitter() # small data set
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() # 5 numbers
ggplot(mpg, aes(drv, hwy)) + geom_violin() # difficult to explain

# Histograms and Frequency Polygons
ggplot(mpg, aes(hwy)) + geom_histogram()

ggplot(mpg, aes(hwy)) + geom_freqpoly()

ggplot(mpg, aes(hwy)) +
  geom_freqpoly(binwidth = 2.5)

ggplot(mpg, aes(hwy)) +
  geom_freqpoly(binwidth = 1)

# for different groups
ggplot(mpg, aes(displ, colour = drv)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ drv, ncol=1)

# Bar Charts
