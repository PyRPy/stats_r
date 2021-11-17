
# Exercise 8.5-10  --------------------------------------------------------
# ordered stats

# read the data
dat <- read.delim("Data415/E8_5-10.txt", header=FALSE)
colnames(dat) = c('x', 'y')
dat

dat_long = c(dat$x, dat$y)
dat_long
dat_order = order(dat_long)
dat_order

# dat_long <- scan('Data415/E8_5-10.txt')
# tree = c(rep('x', 12), rep('y', 12))
# dat_label = data.frame(dat_long, tree)
# dat_label
# dat_order = order(dat_label$dat_long)
# dat_order

W = sum(dat_order[1:12])
nx = 12
ny = 12

mu_W = nx * ( nx + ny + 1) / 2
Z = (W - mu_W) / sqrt(nx*ny*(nx+ny+1)/12) # -2.19



