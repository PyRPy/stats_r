# coil.r, coil experiment, Table 13.12, p463

coil.data <- read.table("coil.txt", header=T)
head(coil.data, 3)
# Create factor variables
coil.data <- within(coil.data, 
   {fBlock = factor(Block); fA = factor(A); 
    fB = factor(B); fC = factor(C) })

# Analysis of variance
options(contrasts = c("contr.sum", "contr.poly"))
model1 <- lm(y ~ fBlock + fA*fB*fC, data=coil.data)
anova(model1)
drop1(model1, ~., test="F")

# Contrast estimates, confidence intervals, and tests
library(lsmeans)
lsmA <- lsmeans(model1, ~ fA)
summary(contrast(lsmA, list(A=c(-1, 1))), infer=c(T,T))

lsmB <- lsmeans(model1, ~ fB)
summary(contrast(lsmB, list(B=c(-1, 1))), infer=c(T,T))

lsmC <- lsmeans(model1, ~ fC)
summary(contrast(lsmC, list(C=c(-1, 1))), infer=c(T,T))

lsmAB <- lsmeans(model1, ~ fB:fA)
summary(contrast(lsmAB, list(AB=c(1,-1,-1, 1)/2)), infer=c(T,T))

lsmAC <- lsmeans(model1, ~ fC:fA)
summary(contrast(lsmAC, list(AC=c(1,-1,-1, 1)/2)), infer=c(T,T))

lsmBC <- lsmeans(model1, ~ fC:fB)
summary(contrast(lsmBC, list(BC=c(1,-1,-1, 1)/2)), infer=c(T,T))

lsmABC <- lsmeans(model1, ~ fC:fB:fA)
summary(contrast(lsmABC, list(ABC=c(-1,1,1,-1,1,-1,-1, 1)/4)), 
        infer=c(T,T))