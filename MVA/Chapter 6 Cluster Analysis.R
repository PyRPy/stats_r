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
