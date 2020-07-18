# more on PCA -------------------------------------------------------------
# read data
places <- read.table("places.txt")
head(places)
tail(places)

names(places) <- c("climate", "housing", "health", "cirme", "trans",
                   "educate", "arts", "recreate", "econ", "id")
hist(places$climate)
hist(places$econ)
hist(log(places$econ)) # better

places_pca <- prcomp(places[, 1:9])
biplot(places_pca)
screeplot(places_pca)

places_pca2 <- prcomp(places[, 1:9], center = TRUE, scale. = TRUE)
biplot(places_pca2, scale = 0)
screeplot(places_pca2)

pca.var <- places_pca2$sdev^2
pve <- pca.var/sum(pca.var)

plot(pve, xlab = "Principal component", 
     ylab = "Proportion of variation explained",
     ylim = c(0, 1), 
     type = 'b')

plot(cumsum(pve), xlab = "Principal component", 
     ylab = "Accumulative Prop. of variation explained",
     ylim = c(0, 1), 
     type = 'b')
places_pca2$sdev

# use FactoMineR 
library(FactoMineR)
places_pca3 <- PCA(places[, 1:9])

# dimdesc(places_pca3)
