# UAV.r, UAV experiment, Table 19.24, p748

uav.data <- read.table("uav.txt", header=T) 
uav.data = within(uav.data, {fA = factor(A); fW = factor(W); 
                             fB = factor(B); fC = factor(C) })
head(uav.data, 3)
str(uav.data)
# Least squares ANOVA
# Set contrast options for correct lsmeans and contrasts
options(contrasts = c("contr.sum", "contr.poly"))
model1 <- aov(Time ~ fA*fB*fC + Error(fA:fW), data=uav.data)
summary(model1)

# Compare 2 levels of fA pairwise
library(lsmeans)
lsmA <- lsmeans(model1, ~ fA)
summary(contrast(lsmA, method="pairwise"), infer=c(T,T))

# Compare 2 levels of fA pairwise given each fB:fC combination
library(lsmeans)
lsmAgBC <- lsmeans(model1, ~ fA | fC:fB)
summary(contrast(lsmAgBC, method="pairwise", adjust="none"), 
        infer=c(T,T), level=0.99)