
# Appendix A A sample session ---------------------------------------------

help.start()
x <- rnorm(50)
y <- rnorm(x)
plot(x, y)
x
rnorm(c(0.5, 0.6))

# see all the objects
ls()
rm(x, y)

# create a data frame
x <- 1:20
w <- 1 + sqrt(x)/2
dummy <- data.frame(x = x, y = x + rnorm(x)*w)
dummy

# plot the data
with(dummy, plot(x, y))

# regression model
fm <- lm(y ~ x, data = dummy)
summary(fm)

fm1 <- lm(y ~ x, data = dummy, weights = 1/w^2)
summary(fm1)

attach(dummy) # make dummy visible in environment
lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))
abline(coef(fm1), col="red")
detach(dummy) # remove data frame from search path

# check for heteroscedasticity
plot(fitted(fm), resid(fm),
     xlab = "fitted values",
     ylab = "residuals",
     main = "residuals vs fitted")

qqnorm(resid(fm), main = "Residuals Rankit Plot")
rm(fm, fm1, lrf, x, dummy) # clear environment


#  measure the speed of light ---------------------------------------------

filepath <- system.file("data", "morley.tab", package = "datasets")
filepath
file.show(filepath)
mm <- read.table(filepath)
mm

mm$Expt <- factor(mm$Expt)
mm$Run <- factor(mm$Run)

attach(mm)

# boxplot for comparison
plot(Expt, Speed, main = "Speed of light data", xlab = "Experiment No.")

# randomized block
fm <- aov(Speed ~ Run + Expt, data = mm)
summary(fm)

# analysis of variance
fm0 <- update(fm, .~ . - Run)
anova(fm0, fm) # not significant

detach(mm)
rm(fm, fm0)


# contour and image plots -------------------------------------------------

x <- seq(-pi, pi, len=50)
y <- x
f <- outer(x, y, function(x, y) cos(y)/(1 + x^2))
oldpar <- par(no.readonly = TRUE)
par(pty = "s")
contour(x, y, f)
contour(x, y, f, nlevels = 15, add = TRUE)

fa <- (f - t(f))/2
contour(x, y, fa, nlevels = 15)
par(oldpar)

image(x, y, f)
image(x, y, fa)
objects()
rm(f, fa, x, y)

# complex arithmetic ---------------------------------------------------------

th <- seq(-pi, pi, len = 100)
z <- exp(1i*th)
par(pty = "s")
plot(z, type = "l")

w <- rnorm(100) + rnorm(100) * 1i
w <- ifelse(Mod(w) > 1, 1/w, w)
plot(w, xlim = c(-1, 1), pch = "+", xlab = "x", ylab = "y")
lines(z)

# uniform distribution
w <- sqrt(runif(100)) * exp(2 * pi * runif(100) * 1i)
plot(w, xlim = c(-1, 1), ylim = c(-1, 1), pch = "+", xlab = "x", ylab = "y")
lines(z)

rm(th, w, z)