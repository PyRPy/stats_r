# Chapter 8 The Analysis of Repeated Measures Data
library(MVA)
data()
demo("Ch-LME")

# 8.1 Introduction --------------------------------------------------------
# write.csv(ex_wide, "ex_wide.csv")
ex_wide
# reshape data
reshape(ex_wide, direction = "long", idvar = "ID",
         varying = colnames(ex_wide)[-(1:2)])

head(timber)
head(plasma)


# 8.2 Linear mixed-effects models for repeated measures -------------------
# 8.2.1 Random intercept and random intercept and slope models for the 
# timber slippage data
summary(lm(loads ~ slippage, data = timber))
with(timber, plot(slippage, loads))

timber.lme <- lme(loads ~ slippage, random = ~1 | specimen, 
                  data = timber, method = "ML")
timber.lme1 <- lme(loads ~ slippage,
                    random = ~slippage | specimen,
                    data = timber, method = "ML")
xyplot(loads ~ slippage | specimen, data = timber, layout = c(4, 2))

# test the random intercept model against the simple linear
# regression model
library(RLRsim)
exactRLRT(timber.lme)

# two random-effect models
anova(timber.lme, timber.lme1)

summary(timber.lme1)
