
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
