# gamma distribution
# The Gamma distribution with parameters shape = a and scale = s has density
# f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)

# Figure 3.2 - 2 Gamma pdfs
N <- 1000
x <- seq(0, 30, by = 0.5)
plot(x, dgamma(x, 1/4, 1/4), type = 'l', ylim = c(0, 0.3))
lines(x, dgamma(x, 1, 1/4), type = 'l', col = 'blue')
lines(x, dgamma(x, 2, 1/4), type = 'l', col = 'green')
lines(x, dgamma(x, 3, 1/4), type = 'l', col = 'yellow')
lines(x, dgamma(x, 4, 1/4), type = 'l', col = 'red')

plot(x, dgamma(x, 4, 6/5), type = 'l', ylim = c(0, 0.3))
lines(x, dgamma(x, 4, 1/1), type = 'l', col = 'blue')
lines(x, dgamma(x, 4, 1/2), type = 'l', col = 'green')
lines(x, dgamma(x, 4, 1/3), type = 'l', col = 'yellow')
lines(x, dgamma(x, 4, 1/4), type = 'l', col = 'red')
# be careful with the shape and scale in gamma function in R
