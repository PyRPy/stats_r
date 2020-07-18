# more on PCA -------------------------------------------------------------
dim(mtcars)

# correlation matrix 
mtcars$cyl <- as.numeric(as.character(mtcars$cyl))
mtcars_correl <- cor(mtcars, use = "complete.obs")

library(ggcorrplot)
ggcorrplot(mtcars_correl)

# use base R prcomp() 
mtcars_pca <- prcomp(mtcars)

# use FactoMineR 
library(FactoMineR)
mtcars_pca2 <- PCA(mtcars)

mtcars_pca2$eig
mtcars_pca2$var$cos2
mtcars_pca2$var$contrib
dimdesc(mtcars_pca2)

# more visualizations
library(factoextra)
# namespace ‘tibble’ 2.1.3 is already loaded, but >= 3.0.0 is required
# not working ...
fviz_pca_var(mtcars_pca2, col.var = "contrib", 
                          repel = TRUE)

mtcars$cyl <- as.factor(mtcars$cyl)
fviz_pca_ind(mtcars_pca2, label = "var", habillage = mtcars$cyl,
             addEllipses = TRUE)
