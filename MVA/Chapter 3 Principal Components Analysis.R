# Chapter 3 Principal Components Analysis
library(MVA)

# 3.10 Some examples of the application of principal ----------------------

# 3.10.3 Air pollution in US cities
cor(USairpollution[,-1])
head(USairpollution)

usair_pca <- princomp(USairpollution[,-1], cor=TRUE)

data("USairpollution", package = "HSAUR2")
head(USairpollution)

panel.hist <- function(x, ...) {
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5) )
   h <- hist(x, plot = FALSE)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$counts; y <- y/max(y)
   rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
   }
USairpollution$negtemp <- USairpollution$temp * (-1)
USairpollution$temp <- NULL

# Scatterplot matrix of six variables in the air pollution data
pairs(USairpollution[,-1], diag.panel = panel.hist,
          pch = ".", cex = 1.5)

summary(usair_pca, loadings = TRUE)

# Bivariate boxplots of the first three principal components
pairs(usair_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
       panel = function(x,y, ...) {
         text(x, y, abbreviate(row.names(USairpollution)),
                cex = 0.6)
         bvbox(cbind(x,y), add = TRUE)
         })

par(mfrow=c(3,2))
out <- sapply(1:6, function(i) {
   plot(USairpollution$SO2,usair_pca$scores[,i],
          xlab = paste("PC", i, sep = ""),
          ylab = "Sulphur dioxide concentration")
   })
par(mfrow=c(1,1))

# linear model using PCA results
usair_reg <- lm(SO2 ~ usair_pca$scores, data = USairpollution)
summary(usair_reg)

usair_lm <- lm(SO2 ~., data = USairpollution)
summary(usair_lm) # no difference in R-squared ?


# 3.11 The biplot ---------------------------------------------------------

biplot(usair_pca, col = c("gray", "black"))

