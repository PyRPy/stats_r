# balloon.r, balloon experiment, Table 9.6 p300, and code from text p301

# Table 9.6, p300
balloon.data <- read.table("Data/balloon.txt", header=T)
head(balloon.data, 3)
balloon.data <- within(balloon.data,
  {x = Order - 16.5; fC = factor(Color) })

options(contrasts = c("contr.sum", "contr.poly"))
model1 <- lm(Time ~ fC + x, data=balloon.data)
summary(model1) # LSE etc. for covariate, model F-test

drop1(model1, ~., test="F") # Type 3 tests
anova(model1) # Type 1 tests

# Multiple comparisons: Scheffe's method
library(lsmeans)
lsmC <- lsmeans(model1, ~ fC)
summary(contrast(lsmC, method="pairwise", adjust="Scheffe"),
        infer=c(T,T))
lsmC

# Residual plots
balloon.data <- within(balloon.data,
  {pred=fitted(model1); e=resid(model1); z=e/sd(e);
   n=length(e); q=rank(e); nscore=qnorm((q-0.375)/(n+0.25)) })
plot (z ~ Order, data=balloon.data); abline(h=0)
plot (z ~ pred, data=balloon.data); abline(h=0)
plot (z ~ Color, data=balloon.data); abline(h=0)

plot (z ~ nscore, data=balloon.data); qqline(balloon.data$z)

# Code from text p301
# Test equality of slopes
model2 <- lm(Time ~ fC + x + fC:x, data=balloon.data)
anova(model1, model2)
# slopes are not too much different