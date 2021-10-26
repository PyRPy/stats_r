# concrete strength
dat <- read.delim("E9_4-03.txt", header=FALSE)
head(dat)
colnames(dat) <- c("A", "B", "Strength")

head(dat)
dat$A <- as.factor(dat$A)
dat$B <- as.factor(dat$B)

# linear model
lm1 <- lm(Strength ~ A + B, data = dat)
summary(lm1)

anova(lm1)
# Analysis of Variance Table
# 
# Response: Strength
#             Df Sum Sq Mean Sq F value    Pr(>F)    
#   A          2   89.2   44.60  7.6239 0.0140226 *  
#   B          4  363.6   90.90 15.5385 0.0007684 ***
#   Residuals  8   46.8    5.85 