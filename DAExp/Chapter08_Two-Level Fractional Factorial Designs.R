
# Chapter 8 Two-Level Fractional Factorial Designs ------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)

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

library(DoE.base)
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

