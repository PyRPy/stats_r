# Chapter 5 Exploratory Factor Analysis
library(MVA)
# 5.9 Two examples of exploratory factor analysis -------------------------
# 5.9.1 Expectations of life
head(life)
# write.csv(life, "life.csv")

## a three-factor solution might be adequate to account for the observed 
## covariances in the data,
sapply(1:3, function(f) factanal(life, factors = f, method ="mle")$PVAL)

factanal(life, factors = 3, method ="mle")
## the first factor is dominated by life expectancy at birth for both 
## males and females

(scores <- factanal(life, factors = 3, 
                    method = "mle",
                    scores = "regression")$scores)

# Individual scatterplots of three factor scores for life expectancy data, 
# with points labelled by abbreviated country names.
plot(scores[,1], scores[,2], type = "n", 
     xlab = "Factor 1", ylab = "Factor 2")
text(scores[,1], scores[,2], abbreviate(rownames(life), 5), cex = cex)

# 5.9.2 Drug use by American college students
head(druguse)
# determine the number of factors using the maximum likeli-hood test
sapply(1:6, function(nf) factanal(covmat = druguse, factors = nf,
                         method = "mle", n.obs = 1634)$PVAL)

## Substances that load highly on the first factor are cigarettes, beer, 
## wine, liquor, and marijuana and we might label it social/soft drug use

(factanal(covmat = druguse, factors = 6, method = "mle", n.obs = 1634))

pfun <- function(nf) {
    fa <- factanal(covmat = druguse, factors = nf,
                    method = "mle", n.obs = 1634)
    est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
    ret <- round(druguse - est, 3)
    colnames(ret) <- rownames(ret) <-abbreviate(rownames(ret), 3)
    ret
}
pfun(6)

## The differences are all very small, underlining that the six-factor 
## model does describe the data very well

# druguse:plot
ord <- order.dendrogram(as.dendrogram(hclust(dist(druguse))))  

panel.corrgram <-    
       function(x, y, z, subscripts, at,  
                               level = 0.9, label = FALSE, ...) 
     {
           require("ellipse", quietly = TRUE)
           x <- as.numeric(x)[subscripts]   
           y <- as.numeric(y)[subscripts]     
           z <- as.numeric(z)[subscripts]   
           zcol <- level.colors(z, at = at, col.regions = grey.colors, ...)   
           for (i in seq(along = z)) {
                 ell <- ellipse(z[i], level = level, npoints = 50,   
                              scale = c(.2, .2), centre = c(x[i], y[i]))
                 panel.polygon(ell, col = zcol[i], border = zcol[i], ...)
             }
           if (label)  
                 panel.text(x = x, y = y, lab = 100 * round(z, 2), cex = 0.8,
                                                 col = ifelse(z < 0, "white", "black"))   
       }    

print(levelplot(druguse[ord, ord], at = do.breaks(c(-1.01, 1.01), 20),
                             xlab = NULL, ylab = NULL, colorkey = list(space = "top"), 
                             scales = list(x = list(rot = 90)),
                             panel = panel.corrgram, label = TRUE))
# somehow not show shapes
