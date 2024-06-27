
# Chapter 8 Two-Level Fractional Factorial Designs ------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)

FrF2(nfactors=4, resolution=3) # fixed resolution
FrF2(nfactors=5, nruns=8) # fixed runs


# Filtration rate experiment ----------------------------------------------
head(Table8.3)
model <-lm(FiltrationRate ~ A + B + C + D + A:B + A:C + A:D, data=Table8.3)
print(summary(model))

# keep the large effects (A, C, D) and remove small ones (B, A:B)
model <-lm(FiltrationRate ~ A + C + D + A:C + A:D, data=Table8.3)
print(summary(model))


# Circuit manufacturing factors impact o yield ----------------------------
head(Table8.5)
str(Table8.5)

model <- aov(Yield ~ . * ., data=Table8.5)
model.ss <- anova(model)[['Sum Sq']]
effect.estimates <- data.frame(
  'EffectEstimate'=2 * coef(model)[-1],
  'SumOfSquares'=model.ss[-1],
  'Contribution'=sprintf('%0.2f%%', 100*model.ss[-1]/sum(model.ss[-1]))
)
effect.estimates # check the term contribution to the overall variances

# check active effects with a half-normal probability plot

vals <- halfnormal(model)

# fit the reduced model
model <- aov(Yield ~ A + B + C + A:B, data=Table8.5)
print(summary(model))

# Checking the diagnostic plots
op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')


# Mold shrinkage test (k= 6-2, IV) ----------------------------------------
# Example 8.4
head(Table8.10)
model <- lm(Shrinkage ~ . * ., data=Table8.10)
print(summary(model))

# to find the main effects

halfnormal(model) # main effects, A, AB, AD

model <- lm(Shrinkage ~ A + B + A:B, data=Table8.10)
print(summary(model))

# check residues and assumpptions
op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=Table8.10$C, y=resid(model), main='C vs. Residual')
par(op)

# model variances
ix <- Table8.10$C == 1
X <- model.matrix(Shrinkage ~ -1 + . * ., data=Table8.10)
Fi.star <- apply(
  X,
  2,
  function(w){
    log(
      sd(resid(model)[w == 1])^2
      /
        sd(resid(model)[w != 1])^2
    )
  }
)
vals <- halfnormal(Fi.star)


# Machinery deviations  ---------------------------------------------------
# Example 8.6 two 8-3 IV design
head(Table8.16)

model <- aov(log(Deviation) ~ Error(Block) + (A + B + C + D + E + F + G + H)^3,
             data=Table8.16)
print(summary(model)) # error - block, error - within

model.ss <- summary(model$Within)[[1]][['Sum Sq']]
coef.est <- coef(model$Within)
effect.estimates <- data.frame(
  'EffectEstimate'=2 * coef.est,
  'SumOfSquares'=model.ss,
  'Contribution'=sprintf('%0.2f%%', 100*model.ss/sum(model.ss))
)
effect.estimates # field knowledge chooses A, B, D, and A:D

model <- aov(log(Deviation) ~ (A + B + C + D + E + F + G + H)^3,
             data=Table8.16) # remove error terms
vals <- halfnormal(model)

# remodel using A, B, D, A:D
model <- aov(log(Deviation) ~ Error(Block) + A + B + D + A:D,
             data=Table8.16)
print(summary(model))


# Eye focus experiment ----------------------------------------------------
# foldover conceppt, 2 k = 7-4, III experiment
# check the main effects
model <- lm(Time ~ ., data=Table8.21)
print(2*coef(model)[-1])

# foldover
df <- rbind(Table8.21, Table8.22)
df
model <- lm(Time ~ (A + B + D)^2, data=df)  # two order interactions
summary(model) # Adjusted R-squared:  0.9889


# Plackett and Burman designs ---------------------------------------------
# 12 factors, 16 run, give us some idea of using minimal number of runs
# to test maximum number of factors with acceptable results
head(Table8.25)

alpha.to.enter <- 0.10
alpha.to.leave <- 0.10
model <- lm(y~1, data=Table8.25)
scope <- y ~ (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)^2
final.model <- p.stepwise(model, scope, alpha.to.enter, alpha.to.leave)
summary(final.model)


# 6 factors impact on the coating thickness -------------------------------
head(Table8.31)
model <- lm(Thickness ~ . * ., data=Table8.31)
vals <- halfnormal(model)

vals$signif

# A:B or C:E are significant, complete fold-over on A
head(Table8.32)
model <- lm(Thickness ~ Blocks + (A + B + C + D + E + F)^2, data=Table8.32)
vals <- halfnormal(model)
vals$signif # C:E significant

# partial fold-over on A at its low level
ix <- Table8.32[,'Blocks'] == 1 | (Table8.32[,'Blocks'] == 2 & Table8.32[,'A'] == -1)
Table8.33 <- Table8.32[ix,]
model <- lm(Thickness ~ Blocks + (A + B + C + D + E + F)^2, data=Table8.33)
vals <- halfnormal(model, ME.partial=TRUE)

# to remove 'block' as main effect
model <- aov(Thickness ~ Error(Blocks) + (A + B + C + D + E + F)^2, data=Table8.33)
print(summary(model)) # A:B interactoin is significant
