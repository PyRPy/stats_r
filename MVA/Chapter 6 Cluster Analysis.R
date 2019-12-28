# Chapter 6 Cluster Analysis
library(MVA)
# demo("Ch-CA")

# 6.3 Agglomerate hierarchical clustering ---------------------------------

dm <- dist(measure[, c("chest", "waist", "hips")])
dm
plot(cs <- hclust(dm, method = "single"))
plot(cc <- hclust(dm, method = "complete"))
plot(ca <- hclust(dm, method = "average"))

body_pc <- princomp(dm, cor = TRUE)
xlim <- range(body_pc$scores[, 1])
plot(body_pc$scores[, 1:2], type="n", xlim=xlim, ylim=xlim)
lab <- cutree(cs, h=3.8)
text(body_pc$scores[, 1:2], labels = lab, cex=0.6)


# 6.3.1 Clusterng jet fighters --------------------------------------------

head(jet)
# write.csv(jet, "jet.csv")

#        FFD   SPR  RGF   PLF SLF CAR
# FH-1   82 1.468 3.30 0.166 0.1  no
# FJ-1   89 1.605 3.64 0.154 0.1  no
# F-86A 101 2.168 4.87 0.177 2.9 yes
# F9F-2 107 2.054 4.72 0.275 1.1  no
# F-94A 115 2.467 4.11 0.298 1.0 yes
# F3D-1 122 1.294 3.75 0.150 0.9  no

# FFD - first flight date
# SPR - specific power
# RGF - flight range factor
# PLF - payload as a fraction of gross weight of aircraft
# SLF - sustained load factor
# CAR - can land on a aircraft carrier 1 or not 0

# clustering - complete
X = scale(jet[, c("SPR", "RGF", "PLF", "SLF")], 
          center = FALSE, scale = TRUE)

dj = dist(X)
plot(cc <-  hclust(dj), main = "Jets clustering")
cc

# PCA analysis
pr <- prcomp(dj)$x[, 1:2]
plot(pr, pch=(1:2)[cutree(cc, k=2)],
     col = c("black", "darkgrey")[jet$CAR],
     xlim = range(pr) * c(1, 1.5))
legend("topright", col=c("black", "black", "darkgrey", "darkgrey"),
       legend = c("1/no", "2/no", "1/yes", "2/yes"),
       pch = c(1:2, 1:2), title = "Cluster/CAR", bty = "n")


# 6.4.1 Clustering the crime rates ----------------------------------------
head(crime)
# write.csv(crime, "crime.csv")
subset(crime, Murder > 15)

sapply(crime, var) # large var, have to scale data
rge <- sapply(crime, function(x) diff(range(x)))
crime_s <- sweep(crime, 2, rge, FUN = "/")
sapply(crime_s, var)

# two groups
kmeans(crime_s, centers = 2)$centers * rge
n <- nrow(crime_s)
wss <- rep(0, 6)
wss[1] <- (n-1)*sum(sapply(crime_s, var))
for (i in 2:6){
  wss[i] <- sum(kmeans(crime_s, centers = i)$withinss)
}

plot(1:6, wss, type="b", xlab="number of groups",
     ylab="within groups sum of squares")

# 6.4.2 Clustering Romano-British pottery ---------------------------------
set.seed(29)
head(pots)
str(pots)
# write.csv(pots, "pots.csv")
pottery_cluster <- kmeans(pots, centers = 3)$cluster
xtabs(~ pottery_cluster + kiln, data = pottery)
pots <- scale(pottery[, colnames(pottery_cluster) != "kiln"], center = FALSE)
pottery_dist <- dist(pots)
library(lattice)
levelplot(as.matrix(pottery_dist), xlab="Pot number",
          ylab = "Pot number") # not working


# 6.6 Displaying clustering solutions graphically -------------------------

library(flexclust)
library(mvtnorm)
set.seed(290875)
x <- rbind(rmvnorm(n=20, mean=c(0,0), sigma=diag(2)),
           rmvnorm(n=20, mean=c(3,3), sigma=0.5*diag(2)),
           rmvnorm(n=20, mean=c(7,6), sigma=0.5*(diag(2) + 0.25)))
k <- cclust(x, k=5, save.data = TRUE)
plot(k, hull=FALSE, col=rep("black", 5), xlab="x", ylab="y")

k <- cclust(x, k=3, save.data = TRUE)
plot(k, hull=FALSE, col=rep("black", 3), xlab="x", ylab="y")           

# stripes display
set.seed(912345654)
x <- rbind(matrix(rnorm(100, sd=0.5), ncol=2),
           matrix(rnorm(100, mean=4, sd=0.5), ncol=2),
           matrix(rnorm(100, mean=7, sd=0.5), ncol=2),
           matrix(rnorm(100, mean=-1.0, sd=0.7), ncol=2),
           matrix(rnorm(100, mean=-4.0, sd=1.0), ncol=2)
           )

c5 <- cclust(x, 5, save.data = TRUE)
stripes(c5, type = "second", col=1)
