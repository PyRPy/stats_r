# cottonspinning.r, cotton-spinning experiment, Tables 10.14-17 pp332-336,
# and code from text p335

# Increase print width for output as in Table 10.14 (not shown in text)
options(width=80, digits=5, scipen=2) 

# Tables 10.14-17, pp332-336
cotton.data <- read.table("cotton.spinning.txt", header=T)
head(cotton.data, 3) 
cotton.data <- within(cotton.data, 
                     {fBlock = factor(Block); fTrtmt = factor(Trtmt);
                     fFlyer = factor(Flyer); fTwist = factor(Twist) })
head(cotton.data)

# Analysis for randomized complete block design
model1 <- lm(Break ~ fBlock + fTrtmt, data=cotton.data)
anova(model1)

library(lsmeans)
lsmTrtmt <- lsmeans(model1, ~ fTrtmt) 
lsmTrtmt
summary(contrast(lsmTrtmt, method="pairwise", infer=c(T,T)))
summary(contrast(lsmTrtmt, 
                 list(Flyer1m2AtComnTwst=c(0.5, 0.5, 0, 0, -0.5, -0.5), 
                      LinTwistOrdFlyer=c(-10, -1, 11, 0, 0, 0))), 
        infer=c(T,T), level=0.95, side="two-sided")

# Analysis with additive main effects
model2 <- lm(Break ~ fBlock + fFlyer + fTwist, data=cotton.data)
drop1(model2, ~., test="F") 

lsmFlyer <- lsmeans(model2, ~ fFlyer) 
lsmFlyer
summary(contrast(lsmFlyer, method="pairwise"), infer=c(T,T))

lsmTwist <- lsmeans(model2, ~ fTwist)
lsmTwist
summary(contrast(lsmTwist, method="pairwise", adjust="bonf"), 
        infer=c(T,T))

# Analysis of covariance: Twist as covariate
model3 <- lm(Break ~ fBlock + fFlyer + Twist, data=cotton.data)
anova(model3) 
drop1(model3, ~., test="F") 
summary(model3)

lsmFlyer <- lsmeans(model3, ~ fFlyer) 
lsmFlyer
summary(contrast(lsmFlyer, method="pairwise"), infer=c(T,T))

# Code from text p335
# Testing LOF of ancova model: 2 ways
anova(model3, model1)
model4 <- lm(Break ~ fBlock + fFlyer + Twist + fTrtmt, data=cotton.data)
anova(model4)
