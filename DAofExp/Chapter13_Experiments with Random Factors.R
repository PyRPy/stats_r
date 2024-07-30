
# Experiments with Random Factors -----------------------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)

options(contrasts=c('unordered'='contr.sum', 'ordered'='contr.poly'))
# A Measurement Systems Capability Study
head(Table13.1)
df <- data.frame(
  'Part'=c(
    Table13.1$PartNumber, Table13.1$PartNumber,
    Table13.1$PartNumber
  ),
  'Operator'=factor(c(
    rep(1, 40), rep(2, 40), rep(3, 40)
  )),
  'Measurement'=c(
    Table13.1$Operator1, Table13.1$Operator2, Table13.1$Operator3
  )
)
head(df)

# fixed effect and random effect - mixed effect
library(lme4)
library(lmerTest)
model <- lmer(Measurement ~ Operator + (1 | Part) + (1 | Part:Operator),
              data=df, REML=TRUE)
print(anova(model))
print(summary(model))
print(rand(model))


# pressure drop measurement
df<-Table13.9
df[,'GasTemperature']<-factor(df[,'GasTemperature'])
colnames(df)<-c('A','B','C','PressureDrop')
head(df)

model<-lmer(PressureDrop ~ A+(1|B)+(1|C)+(1|A:B)+(1|A:C)+(1|B:C)+(1|A:B:C),
            data=df,REML=TRUE)
print(summary(model))
print(rand(model)) # A:B interaction significant only
