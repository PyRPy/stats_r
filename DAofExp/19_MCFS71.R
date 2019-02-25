# MCFS71.r, mobile computing field study, Tables 19.30-31 pp757-8,
# and code from text p758

# Table 19.30, p757
MCFS71.data <- read.table("MCFS71.txt", header=T) 
head(MCFS71.data, 3)
MCFS71.data <- within(MCFS71.data, {fSubj = factor(Subj); 
       fDay = factor(Day); fOrder = factor(Order); fO2 = factor(O2); 
       fO3 = factor(O3); fPath = factor(Path); fP2 = factor(P2); 
       fP3 = factor(P3); fA = factor(A); fB = factor(B) })
# head(MCFS71.data, 3)

# ReML
library(lmerTest) 
model1 <- lmer(y ~ fO2 + fP2 + fA + fO3 + fO2:fO3 + fP3 + fP2:fP3
                  + fB + fA:fB + (1|fSubj) + (1|fSubj:fDay), 
                  data=MCFS71.data)
summary(model1)
anova(model1) 


# Table 19.31, p758
# Consolidating tests for Order and Path
model2 <- lmer(y ~ fOrder + fPath + fA + fB + fA:fB + (1|fSubj) 
                       + (1|fSubj:fDay), data=MCFS71.data)
anova(model2)

# Multiple comparisons
library(lsmeans)
lsmA <- lsmeans(model1, ~ fA)
summary(contrast(lsmA, method="pairwise", adjust="none"), 
        infer=c(T,T), level=0.95)
lsmB <- lsmeans(model1, ~ fB)
summary(contrast(lsmB, method="pairwise", adjust="none"), 
        infer=c(T,T), level=0.95)

# Code from text p758
lsmAB <- lsmeans(model1, ~ fA:fB) 
summary(contrast(lsmAB, method="pairwise", adjust="bon"), 
        infer=c(T,T), level=0.95)
