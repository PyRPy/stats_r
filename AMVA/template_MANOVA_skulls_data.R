# Analysis of variance ----------------------------------------------------

data(skulls, package = "HSAUR3")
means <- aggregate(skulls[,c("mb", "bh", "bl", "nh")], 
                   list(epoch=skulls$epoch), mean)
means

pairs(means[,-1], panel = function(x, y) {
  text(x, y, abbreviate(levels(skulls$epoch)))
})


# MANOVA ------------------------------------------------------------------

skulls_manova <- manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls)
summary(skulls_manova, test = "Pillai")
summary(skulls_manova, test = "Wilks")

summary.aov(skulls_manova)

summary(manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls,
               subset = epoch %in% c("c4000BC", "c3300BC")))

# reference
# https://cran.r-project.org/web/packages/HSAUR/vignettes/Ch_analysis_of_variance.pdf
