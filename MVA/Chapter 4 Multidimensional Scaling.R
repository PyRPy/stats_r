# Chapter 4 Multidimensional Scaling
library(MVA)
# 4.4 Classical multidimensional scaling ----------------------------------

# 4.4.2 Examples of classical multidimensional scaling

# Airline distances between ten US cities
airdist
# write.csv(airline.dist, "airlinedist.csv")
airline_mds <- cmdscale(airdist, k=9, eig = TRUE)
airline_mds$points

(lam <- airline_mds$eig)
cumsum(abs(lam)) / sum(abs(lam))
cumsum(lam^2) / sum(lam^2)

# Two-dimensional classical MDS solution for airline distances
lim <- range(airline_mds$points[,1] * (-1)) * 1.2
plot(airline_mds$points[,1] * (-1), airline_mds$points[,2] * (-1),
           type = "n", xlab = "Coordinate 1", ylab = "Coordinate 2",
           xlim = lim, ylim = lim)
text(airline_mds$points[,1] *(-1), airline_mds$points[,2] * (-1), 
           labels(airdist), cex = 0.7)


# 4.5 Non-metric multidimensional scaling ---------------------------------
# 4.5.1 House of Representatives voting
library(MASS)
data(voting, package = "HSAUR2")
head(voting)
voting_mds <- isoMDS(voting)

# Two-dimensional solution from non-metric multidimensional scaling of
# distance matrix for voting matrix
x <- voting_mds$points[,1]
y <- voting_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
         xlim = range(voting_mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(voting), cex = 0.6)
voting_sh <- Shepard(voting[lower.tri(voting)],
                         voting_mds$points)

# The Shepard diagram for the voting data shows some discrepancies be-tween 
# the original dissimilarities and the multidimensional scaling solution.
plot(voting_sh, pch = ".", xlab = "Dissimilarity",
         ylab = "Distance", xlim = range(voting_sh$x),
         ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")


# 4.6 Correspondence analysis ---------------------------------------------
# 4.6.1 Teenage relationships
teensex
D <- function(x) {
  a <- t(t(x) / colSums(x))
  ret <- sqrt(colSums((a[,rep(1:ncol(x), ncol(x))] -
                           a[, rep(1:ncol(x), rep(ncol(x), ncol(x)))])^2 *
                           sum(x) / rowSums(x)))
  matrix(ret, ncol = ncol(x))
  }
(dcols <- D(teensex))
(drows <- D(t(teensex)))

# Correspondence analysis for teenage relationship data.
r1 <- cmdscale(dcols, eig = TRUE)
c1 <- cmdscale(drows, eig = TRUE)
plot(r1$points, xlim = range(r1$points[,1], c1$points[,1]) * 1.5,
         ylim = range(r1$points[,1], c1$points[,1]) * 1.5, type = "n",
         xlab = "Coordinate 1", ylab = "Coordinate 2", lwd = 2)
text(r1$points, labels = colnames(teensex), cex = 0.7)
text(c1$points, labels = rownames(teensex), cex = 0.7)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
