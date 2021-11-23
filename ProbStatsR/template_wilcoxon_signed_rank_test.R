# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/wilcoxon-signed-rank-test

library(MASS)         
head(immer) 

boxplot(immer$Y1, immer$Y2)

wilcox.test(immer$Y1, immer$Y2, paired=TRUE) 

# Welch t test
t.test(immer$Y1, immer$Y2)
