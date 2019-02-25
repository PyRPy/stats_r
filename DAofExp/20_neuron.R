# neuron.r, neuron experiment, Table 20.7, p785

neuron <- read.table("neuron.txt", header = T)
head(neuron, 3)
# install.packages("mlegp")
library(mlegp)
gasp <- mlegp(neuron[, 1:2], neuron[, 3])
summary(gasp)
predictedX <- expand.grid(g_NaF = seq(0, 1, 0.01), 
                          g_KDR = seq(0, 1, 0.01))
yhats <- predict(gasp, predictedX)
# install.packages("rgl")
library(rgl)
plot3d(neuron[, 1], neuron[, 2], neuron[, 3], 
       col = "red", size = 3, type = "s", 
       xlab = "g NaF (mS/cm^2)", ylab = "g KDR (mS/cm^2)",
       zlab = "Firing rate (spikes per 2 s)")
plot3d(predictedX[, 1], predictedX[, 2], yhats, col = "blue", 
       size = 0.5, type = "s", add = TRUE) 
