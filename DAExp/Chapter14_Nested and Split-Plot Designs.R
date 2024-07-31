
# Nested and Split-Plot Designs -------------------------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)


# Purities test from different suppliers ----------------------------------

library(lme4)
library(lmerTest)

head(Table14.3)
model <- lmer(Purity ~ (1 | Supplier/Batches), data=Table14.3)
print(summary(model))

print(rand(model)) # most variance comes from within suppliers
