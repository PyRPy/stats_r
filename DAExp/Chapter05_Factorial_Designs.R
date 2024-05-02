
# Chapter 5 Factorial Designs ---------------------------------------------

# life of batteries affected by materials, temperature

library(MASS)
library(MontgomeryDAE)
head(Table5.1)

df <- Table5.1
df$Temperature <- factor(df$Temperature)

# a partial F-test
null.model <- aov(BatteryLife~1, data=df)
full.model <- aov(BatteryLife ~ MaterialType + Temperature +
                    MaterialType:Temperature, data=df)
anova(null.model, full.model)

with(df, {
  interaction.plot(Temperature, MaterialType, BatteryLife)
})

# check the model assumptions
qqnorm(resid(full.model), main='Normal Probability Plot')
qqline(resid(full.model), col='orange')
plot(x=predict(full.model), y=resid(full.model), main='Fitted vs. Residual')


# model temperature as quadratic effects
df <- Table5.1
df$Temperature <- (df$Temperature - 70) / 55
df$MaterialType <- factor(df$MaterialType)
contrasts(df$MaterialType) <- as.matrix(cbind(c(1,0,-1), c(0,1,-1)))
model <- lm(BatteryLife ~ Temperature + MaterialType +
             I(Temperature^2) + Temperature:MaterialType +
             I(Temperature^2):MaterialType, data=df)
anova(model)
summary(model)


# target detection in a radar scope ---------------------------------------

head(Table5.21)
# operator is blocked

model <- aov(Intensity ~ Error(Operator) + GroundClutter * FilterType,
             data=Table5.21)
summary(model)

# model it as random effects
library(lme4)
library(lmerTest)
model <- lmer(
  Intensity ~ GroundClutter * FilterType + (1|Operator),
  REML=TRUE,
  data=Table5.21
)
print(summary(model))
print(rand(model))
