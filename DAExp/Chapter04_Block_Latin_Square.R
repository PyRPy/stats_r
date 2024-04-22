
# chapter 4 Randomized Blocks, Latin Squares, and Related Designs ---------


# https://github.com/ehassler/MontgomeryDAE/tree/master

library(MASS)
library(MontgomeryDAE)

head(Table4.3)

# include batch as error term, noisse reduction
model <- aov(Flicks ~ Error(BatchOfResin) + ExtrusionPressure, data=Table4.3)
print(summary(model)) # F value = 8.1

# if consider treatment is complete random design
summary(aov(Flicks ~ ExtrusionPressure, data=Table4.3)) # F value = 3.9

# a boxplot for the defects
boxplot(Flicks ~ ExtrusionPressure, data=Table4.3)

# residue and assumptions check
model <- aov(Flicks ~ ExtrusionPressure + BatchOfResin, data=Table4.3)
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')

# assume the blocks are random effects,
library(lme4)
library(lmerTest)
model <- lmer(
  Flicks ~ -1 + ExtrusionPressure + (1|BatchOfResin),
  data=Table4.3,
  REML=TRUE
)

print(summary(model))
print(rand(model))
confint(model, oldNames=FALSE)
