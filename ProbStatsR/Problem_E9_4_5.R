# food consumption rate by rats

dat <- read.delim("E9_4-05.txt", header=FALSE)
head(dat)
colnames(dat) <- c("A", "B", "Food")

head(dat)
dat$A <- as.factor(dat$A)
dat$B <- as.factor(dat$B)

# linear model with interaction
lm1 <- lm(Food ~ A + B + A:B, data = dat)
summary(lm1)

anova(lm1)
# Analysis of Variance Table
# 
# Response: Food
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#   A          1  29.28  29.275  5.5328  0.02498 *  
#   B          3 454.70 151.566 28.6451 3.44e-09 ***
#   A:B        3  27.34   9.115  1.7226  0.18206    
#   Residuals 32 169.32   5.291 

# linear model without interaction
lm2 <- lm(Food ~ A + B, data = dat)

anova(lm2)
# Response: Food
#             Df Sum Sq Mean Sq F value    Pr(>F)    
#   A          1  29.28  29.275  5.2101   0.02864 *  
#   B          3 454.70 151.566 26.9744 3.223e-09 ***
#   Residuals 35 196.66   5.619 