
# Chapter 7 Blocking and confounding in 2k design.R -----------------------

library(MASS)
library(MontgomeryDAE)
df <- MontgomeryDAE::Figure6.1
df[,'Block'] <- factor(c(rep(1, 4), rep(2, 4), rep(3, 4)))
head(df)

model <- aov(Yield ~ Error(Block) + A * B, data=df) # block as fixed effect
summary(model)

library(lme4)
library(lmerTest)
model <- lmer(
  Yield ~ A * B + (1|Block),
  data=df,
  REML=TRUE
)
summary(model) # blocking not significant

rand(model)
confint(model, oldNames=FALSE) #main effects and for the variance components


# Filtration example with blocking ----------------------------------------
library(DoE.base)
head(Table6.10)
df <- Table6.10
df[df$Block == 1, 'Filtration'] <- df[df$Block == 1, 'Filtration'] - 20

model <- aov(Filtration ~ Temperature * Pressure * Formaldehyde * StirringRate,
             data=Table6.10)
model.ss <- anova(model)[['Sum Sq']]
effect.estimates <- data.frame(
  'EffectEstimate'=2 * coef(model)[-1],
  'SumOfSquares'=model.ss[-1],
  'Contribution'=sprintf('%0.2f%%', 100*model.ss[-1]/sum(model.ss[-1]))
)

# display the table
effect.estimates

# choose the largest contributions and reduce the model.
# include blocking into error terms

model <- aov(Filtration ~ Error(Block) + Temperature + Formaldehyde +
               StirringRate + Temperature:Formaldehyde +
               Temperature:StirringRate, data=df)
summary(model)


# plasma etching experiment -----------------------------------------------

head(Table6.4)
model <- aov(EtchRate ~ Error(Block) + Gap * Flow * Power, data=Table6.4)
summary(model)
