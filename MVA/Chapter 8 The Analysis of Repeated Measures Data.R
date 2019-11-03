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

timber$pred1 <- predict(timber.lme1)

# second order of regression
timber.lme2 <- lme(loads ~ slippage + I(slippage^2),
                    random = ~slippage | specimen,
                    data = timber, method = "ML")
anova(timber.lme1, timber.lme2)

pfun <- function(x, y) {
   panel.xyplot(x, y[1:length(x)])
   panel.lines(x, y[1:length(x) + length(x)], lty = 1)
}

plot(xyplot(cbind(loads, pred1) ~ slippage | specimen,
             data = timber, panel = pfun, layout = c(4, 2),
             ylab = "loads"))

# 8.2.3 Fitting random-effect models to the glucose challenge data
x <- reshape(plasma, direction = "wide", timevar = "time",
              idvar = "Subject", v.names = "plasma")

# Glucose challenge data for control and obese groups over time. Each line
# represents the trajectory of one individual.
parallel(~ x[,-(1:2)] | group, data = x, horizontal = FALSE,
          col = "black", scales = list(x = list(labels = 1:8)),
          ylab = "Plasma inorganic phosphate",
          xlab = "Time (hours after oral glucose challenge)")

plasma.lme1 <- lme(plasma ~ time + I(time^2) + group,
                       random = ~ time | Subject,
                       data = plasma, method = "ML")

summary(plasma.lme1)
# group effect is obtained from 0.437±1.96×0.186, giving [???3.209, 4.083]


plot(splom(~ x[, grep("plasma", colnames(x))] | group, data = x,
            cex = 1.5, pch = ".", pscales = NULL, varnames = 1:8))

# The independence model can be fitted in the usual way with the 
# lm() function

summary(lm(plasma ~ time + I(time^2) + group, data = plasma))

# the standard error for the group effect is about one-half of that 
# given for model plasma.lme1 and if used would lead to a much stronger 
# claim of evidence of a difference between control and obese patients

plasma.lme2 <- lme(plasma ~ time*group +I(time^2),
                    random = ~time | Subject,
                    data = plasma, method = "ML")

anova(plasma.lme1, plasma.lme2)
summary(plasma.lme2)

res.int <- random.effects(plasma.lme2)[,1]
res.slope <- random.effects(plasma.lme2)[,2]
