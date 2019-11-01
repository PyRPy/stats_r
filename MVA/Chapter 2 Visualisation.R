# Chapter 2 Looking at Multivariate Data: Visualisation

# 2.1 Introduction --------------------------------------------------------
# To provide an overview;
# To tell a story;
# To suggest hypotheses;
# To criticise a model


# 2.2 The scatterplot -----------------------------------------------------
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab)

# 2.2.1 The bivariate boxplot

plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab)
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)


# Scatterplot of manu and popul that shows the marginal 
# distributions by histogram and boxplot.
layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),
          widths = c(2, 1), heights = c(1, 2), respect = TRUE)
xlim <- with(USairpollution, range(manu)) * 1.1
plot(popul ~ manu, data = USairpollution, cex.lab = 0.9,
        xlab = mlab, ylab = plab, type = "n", xlim = xlim)
with(USairpollution, text(manu, popul, cex = 0.6,
        labels = abbreviate(row.names(USairpollution))))
with(USairpollution, hist(manu, main = "", xlim = xlim))
with(USairpollution, boxplot(popul))
dev.off()

# Scatterplot of manu and popul showing the bivariate boxplot of the data
outcity <- match(lab <- c("Chicago", "Detroit", "Cleveland", "Philadelphia"), 
                          rownames(USairpollution))
x <- USairpollution[, c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
text(x$manu[outcity], x$popul[outcity], labels = lab,
         cex = 0.7, pos = c(2, 2, 4, 2, 2))


with(USairpollution, cor(manu, popul))
outcity <- match(c("Chicago", "Detroit",
                   "Cleveland", "Philadelphia"),
                   rownames(USairpollution))

with(USairpollution, cor(manu[-outcity], popul[-outcity]))


# 2.2.2 The convex hull of bivariate data
(hull <- with(USairpollution, chull(manu, popul)))
with(USairpollution,
     plot(manu, popul, pch = 1, xlab = mlab, ylab = plab))
with(USairpollution,
     polygon(manu[hull], popul[hull], density = 15, angle = 30))

with(USairpollution, cor(manu[-hull],popul[-hull]))


# 2.2.3 The chi-plot
with(USairpollution, plot(manu, popul,
                             xlab = mlab, ylab = plab,
                             cex.lab = 0.9))
with(USairpollution, chiplot(manu, popul))


# 2.3 The bubble and other glyph plots ------------------------------------
# Bubble plot of temp, wind, and SO2
ylim <- with(USairpollution, range(wind)) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution,
         xlab = "Average annual temperature (Fahrenheit)",
         ylab = "Average annual wind speed (m.p.h.)", pch = 10,
         ylim = ylim)
with(USairpollution, symbols(temp, wind, circles = SO2,
                                 inches = 0.5, add = TRUE))


plot(wind ~ temp, data = USairpollution,
      xlab = "Average annual temperature (Fahrenheit)",
      ylab = "Average annual wind speed (m.p.h.)", pch = 10,
      ylim = ylim)
with(USairpollution,
         stars(USairpollution[,-c(2,5)], locations = cbind(temp, wind),
         labels = NULL, add = TRUE, cex = 0.5))

stars(USairpollution, cex = 0.55)


# 2.4 The scatterplot matrix ----------------------------------------------
pairs(USairpollution, pch = ".", cex = 1.5)

round(cor(USairpollution), 4)

# Scatterplot matrix of the air pollution data showing the linear 
# fit of each pair of variables.
pairs(USairpollution,
         panel = function (x, y, ...)
           {points(x, y, ...)
           abline(lm(y ~ x), col = "grey")
           }, pch = ".", cex = 1.5)



# 2.5 Enhancing the scatterplot with estimated bivariate ------------------

# Three commonly used kernel functions
rec <- function(x) (abs(x) < 1) * 0.5
tri <- function(x) (abs(x) < 1) * (1 - abs(x))
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, rec(x), type = "l", ylim = c(0,1), lty = 1,
         ylab = expression(K(x)))
lines(x, tri(x), lty = 2)
lines(x, gauss(x), lty = 3)
legend("topleft", legend = c("Rectangular", "Triangular",
                  "Gaussian"), lty = 1:3, title = "kernel functions",
                  bty = "n")


# Kernel estimate showing the contributions of Gaussian kernels
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)

h <- 0.4
bumps <- sapply(x, function(a) gauss((xgrid - a)/h)/(n * h))

plot(xgrid, rowSums(bumps), ylab = expression(hat(f)(x)),
         type = "l", xlab = "x", lwd = 2)
rug(x, lwd = 2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))


# 2.6 Three-dimensional plots ---------------------------------------------
epa <- function(x, y) ((x^2 + y^2) < 1) * 2/pi * (1 - x^2 - y^2)
x <- seq(from = -1.1, to = 1.1, by = 0.05)
epavals <- sapply(x, function(a) epa(a, x))
persp(x = x, y = x, z = epavals, xlab = "x", ylab = "y",
          zlab = expression(K(x, y)), theta = -35, axes = TRUE,
          box = TRUE)

# Scatterplot of the log of light intensity and log of surface temperature
# for the stars in star cluster CYG OB1
library("KernSmooth")
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
plot(CYGOB1, xlab = "log surface temperature",
             ylab = "log light intensity")
contour(x = CYGOB1d$x1, y = CYGOB1d$x2,
        z = CYGOB1d$fhat, add = TRUE)
head(CYGOB1)

persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
       xlab = "log surface temperature",
       ylab = "log light intensity",
       zlab = "density")


# 2.7 Trellis graphics ----------------------------------------------------
library("scatterplot3d")
# A three-dimensional scatterplot for the body measurements data with
# points corresponding to male and triangles to female measurements
with(measure, scatterplot3d(chest, waist, hips,
              pch = (1:2)[gender], type = "h", angle = 55))

# A three-dimensional scatterplot for the air pollution data
with(USairpollution,
     scatterplot3d(temp, wind, SO2, type = "h", angle = 55))


# 2.8 Stalactite plots ----------------------------------------------------
plot(xyplot(SO2 ~ temp| cut(wind, 2), data = USairpollution))

# Three-dimensional plots of temp, wind, and precip conditioned on levels
# of SO2.
pollution <- with(USairpollution, equal.count(SO2,4))
plot(cloud(precip ~ temp * wind | pollution, panel.aspect = 0.9,
               data = USairpollution))

plot(xyplot(lat ~ long| cut(depth, 3), data = quakes,
             layout = c(3, 1), xlab = "Longitude",
            ylab = "Latitude"))


# 2.9 Summary -------------------------------------------------------------
plot(cloud(depth ~ lat * long | mag, data = quakes,
            zlim = rev(range(quakes$depth)),
            screen = list(z = 105, x = -70), panel.aspect = 0.9,
            xlab = "Longitude", ylab = "Latitude", zlab = "Depth"))

