
# two way anova -----------------------------------------------------------
# Ha : a1 = a2 = 0
# Hb : b1 = b2 = b3 = 0
# Hab: no interaction


# read the data set

dat <- read.delim("Ex9_4-2.txt", header=FALSE)
names(dat) <- c("A", "B", "Numbers")

dat$A <- as.factor(dat$A)
dat$B <- as.factor(dat$B)

# construct a linear model with interaction term: A:B
mod_lm <- lm(Numbers ~ A + B + A:B, data = dat)
summary(mod_lm)

# interaction is significant
anova(mod_lm)

#           Df Sum Sq Mean Sq  F value  Pr(>F)
# A           1   48.7    48.7   2.8849 0.09246 .
# B           2 8022.7  4011.4 237.7776 < 2e-16 ***
# A:B         2  185.9    93.0   5.5103 0.00534 **
# Residuals 102 1720.8    16.9

