# uniform distribution
u <- runif(200)
hist(u)
mean(u)
var(u)
# compare to theoretical number for variance = 1/12
1/12 # 0.08333333

# alternatively
floor(u*100)

# use min and max range
runif(10, min=0, max=101)
floor(runif(10, min=0, max=101))

# use sample()
sample(1:100, 10, replace = FALSE)

# http://www.cookbook-r.com/Numbers/Generating_random_numbers/
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Uniform.html