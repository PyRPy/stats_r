
# Approximate binomial by normal distribution -----------------------------
# Example 5.7-2
y <- seq(0, 10)
pdf_bi <- dbinom(y, 10, 1/2)
plot(y, pdf_bi, type = 'h')

# approximate binomial using normal
mu <- 10 * 1/2
sigma2 <- 10 * 1/2 * (1-1/2)
pdf_norm <- dnorm(y, mean = mu, sd = sqrt(sigma2)) 
lines(y, pdf_norm, col = 'blue')
