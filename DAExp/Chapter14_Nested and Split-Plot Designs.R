
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


# Circuit insertion -  assembly time vs fixture, layout, operator ---------
head(Table14.10)
df <- Table14.10
df[,'FO'] <- factor(sprintf('F%s.O%s', as.character(df[,'Fixture']),
                            as.character(df[,'Operator'])))
head(df)
# pay attention to the levels of the data structure
model <- lmer(AssemblyTime ~ Layout * Fixture + (1|Operator/Layout) +
                (1|FO/Layout), data=df)
print(anova(model))
print(summary(model))
print(rand(model)) # none is significant?



# Strength of paper -------------------------------------------------------
# factors : prepparation method, replicates, temperature
head(Table14.16)
df<-Table14.16
df[,'Temperature']<-factor(df[,'Temperature'])

# fixed effect model
model<-aov(TensileStrength ~ Temperature*Preperation + Error(Replicate),data=df)
summary(model)

# mixed effect model - different than in textbook
model<-lmer(TensileStrength~Temperature*Preperation*(1|Replicate),data=df)
print(anova(model))
print(summary(model))
print(rand(model))
