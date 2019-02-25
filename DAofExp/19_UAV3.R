# UAV3.r, UAV experiment, Tables 19.26-27, pp751-2

# Table 19.26, p751
uav3.data <- read.table("uav3.txt", header=T) 
uav3.data <- within(uav3.data, 
                   {fA = factor(A); fW = factor(W); fB = factor(B) })
head(uav3.data, 3)


# ANOVA: full model
# Set contrast options for correct lsmeans and contrasts
options(contrasts = c("contr.sum", "contr.poly"))
model1 <- aov(Time ~ fA + fB + fA:fB + Error(fA:fW), data=uav3.data)
summary(model1)

# Multiple comparisons
library(lsmeans)
lsmA.aov <- lsmeans(model1, ~ fA)
summary(contrast(lsmA.aov, method="pairwise"),
        infer=c(T,T), side="two-sided")
lsmAB.aov <- lsmeans(model1, ~ fA:fB) 
summary(contrast(lsmAB.aov, method="pairwise", adjust="tukey"), 
        infer=c(T,T), side="two-sided")

# Table 19.27, p752
# ReML: full model
library(lmerTest) 
model2 <- lmer(Time ~ fA + fB + fA:fB + (1|fA:fW), data=uav3.data)
anova(model2) 
summary(model2) # Gives fA:fW variance component estimate approx 0

# Multiple comparisons
# Detach then reload lsmeans, to avoid issues from its masking by lmerTest
detach("package:lsmeans", unload=TRUE)
library(lsmeans)
lsmA2 <- lsmeans(model2, ~ fA)
summary(contrast(lsmA2, method="pairwise"),
        infer=c(T,T), side="two-sided")
lsmAB.reml <- lsmeans(model2, ~ fA:fB)
summary(contrast(lsmAB.reml, method="pairwise", adjust="tukey"),
        infer=c(T,T), side="two-sided")

# ANOVA: reduced model--without fW
model3 <- aov(Time ~ fA + fB + fA:fB, data=uav3.data)
summary(model3)

# Multiple comparisons
lsmA.aov2 <- lsmeans(model3, ~ fA)
summary(contrast(lsmA.aov2, method="pairwise"),
        infer=c(T,T), side="two-sided")
lsmAB.aov2 <- lsmeans(model3, ~ fA:fB)
summary(contrast(lsmAB.aov2, method="pairwise", adjust="tukey"),
        infer=c(T,T), side="two-sided")
