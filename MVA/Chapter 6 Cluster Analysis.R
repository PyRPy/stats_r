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
