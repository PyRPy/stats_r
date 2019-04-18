# oats2.r, oats experiment, Tables 19.28-9 pp 754-5,
# and code from text pp753+755

# Table 19.28, p754, intra-block analysis
oats.data <- read.table("Data/oats.txt", header=T) 
oats.data <- within(oats.data, {fBlock = factor(Block); 
             fWP = factor(WP); fA = factor(A); fB = factor(B) })

# Reduce data so level of A constitute a BIBD
oats2.data <- subset(oats.data, 
                       !(fA == 0 & (fBlock == 3 | fBlock == 5))
                       & !(fA == 1 & (fBlock == 1 | fBlock == 4))
                       & !(fA == 2 & (fBlock == 2 | fBlock == 6)) )

# Intra-block analysis: ANOVA, fixed block effects
options(contrasts = c("contr.sum", "contr.poly")) # For correct lsmeans
model1 <- aov(y ~ fBlock + fA + fB + fA:fB + Error(fWP:fBlock), 
             data=oats2.data)
summary(model1) 

# Dunnett's Method for A: ANOVA approach
library(lsmeans)
lsmA1 <-  lsmeans(model1, ~ fA)
set.seed(19831957)
summary(contrast(lsmA1, method="trt.vs.ctrl", adjust="mvt"), 
        infer=c(T,T), level=0.99)


# Code from text p753
# Intra-block analysis: ReML, fixed block effects
library(lmerTest) 
model2 <- lmer(y ~ fBlock + fA + fB + fA:fB + (1|fWP:fBlock), 
              data=oats2.data)
anova(model2) 


# Table 19.29, p755
# Recovering inter-block information: ReML, random block effects
model3 <- lmer(y ~ fA + fB + fA:fB + (1|fBlock) + (1|fWP:fBlock), 
              data=oats2.data)
anova(model3) 

# Dunnett's Method for A: ReML approach
# Detach then reload lsmeans, to avoid issues from its masking by lmerTest
detach("package:lsmeans", unload=TRUE)
library(lsmeans)
lsmA3 <- lsmeans(model3, ~ fA)
set.seed(19831957)
summary(contrast(lsmA3, method="trt.vs.ctrl", adjust="mvt"), 
        infer=c(T,T), level=0.99)


# Code from text p755
# The ANOVA approach gives 2 tests for A, corresponding to 2 of the 3 
# error strata, rather than giving one composite test.
# Recovering inter-block information: ANOVA, random block effects
model4 <- aov(y ~ fA + fB + fA:fB + Error(fBlock + fWP:fBlock), 
             data=oats2.data)
summary(model4) 