# oats.r, oats experiment, Tables 19.22-3, pp746-7

# Table 19.22, p746
oats.data <- read.table("oats.txt", header=T) 
oats.data = within(oats.data, {fBlock = factor(Block); 
     fWP = factor(WP); fA = factor(A); fB = factor(B); })
head(oats.data, 3)

# Least squares ANOVA
# Set contrast options for correct lsmeans and contrasts
options(contrasts = c("contr.sum", "contr.poly"))
model1 <- aov(y ~ fA + fB + fA:fB + Error(fBlock + fWP:fBlock), 
             data=oats.data)
summary(model1)

# Table 19.23, p747
# Multiple comparisons: Dunnett's method
library(lsmeans) 
lsmA <- lsmeans(model1, ~ fA) 
set.seed(21531957)
summary(contrast(lsmA, method="trt.vs.ctrl", adjust="mvt", ref=1), 
        infer=c(T,T), level=0.99, side="two-sided") 
lsmB <- lsmeans(model1, ~ fB) 
set.seed(21531957)
summary(contrast(lsmB, method="trt.vs.ctrl", adjust="mvt", ref=1), 
        infer=c(T,T), level=0.99, side="two-sided")
