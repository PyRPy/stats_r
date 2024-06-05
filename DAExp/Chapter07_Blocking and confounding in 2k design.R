
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

