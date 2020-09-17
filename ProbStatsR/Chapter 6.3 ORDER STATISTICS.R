
# 6.3 ORDER STATISTICS ----------------------------------------------------


# Example 6.3-5 q-q plot, N(0, 1) quantiles versus grain diameters --------

grains <- scan("Data-R/Ex6_3-5.txt")
grains
qqnorm(grains)
qqline(grains, col = "blue", lwd = 2)



# steps to construct QQ-Plot
mu <- mean(grains)
sigma2 <- sd(grains)^2

prob <- seq(1, 30, 1)/31
qp <- quantile(grains)

z1p <- qnorm(1-prob, mean=0, sd=1)
z1p

plot(sort(grains), sort(z1p))
abline(h=0, col="blue")
# lines(sort(rnorm(30, 1.33, 0.0040)), sort(z1p))

# if normalized similar to Figure 6.3-2 
xz <- scale(grains)
plot(sort(xz), sort(z1p))
abline(a=0, b=1, col="blue")
abline(h=0)
