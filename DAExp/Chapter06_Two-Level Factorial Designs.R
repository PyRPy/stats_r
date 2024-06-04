
# Two-Level Factorial Designs ---------------------------------------------
library(MASS)
library(MontgomeryDAE)

# construct factorial combinations
expand.grid('A'=c(-1,1), 'B'=c(-1, 1), 'C'=c(-1, 1))

# grid for k factors
fac2 <- function(k){
  as.data.frame(do.call(
    expand.grid,
    lapply(
      1:k,
      function(.){
        c(-1,1)
      }
    )
  ))
}
fac2(2)
fac2(3)


# Experiment on yield ~ concentration + catalyst loading ------------------

str(Figure6.1)
model <- aov(Yield ~ A*B, data=Figure6.1)
summary(model)

# what's inside linear model
model.matrix(~A*B, data=Figure6.1)

# Diagnostic plots
model <- lm(Yield ~ A*B, data=Figure6.1)
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')

# contour plot of the response surface
# maybe 3-D plot is better
model <- lm(Yield ~ A + B, data=Figure6.1)
summary(model)

plot(x=Figure6.1$A, y=Figure6.1$B, main='Contours for Quadratic Model', pch=19)
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
df <- expand.grid('A'=x, 'B'=y)
z <- matrix(predict(model, df), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)


# Plasma Etching ----------------------------------------------------------
# 2 x 2 x 2 design
head(Table6.4)
model <- aov(EtchRate ~ Gap * Flow * Power, data=Table6.4)
summary(model)

# effect estimates
2*coef(model)[-1]


# Filtration Rate ---------------------------------------------------------
head(Table6.10)

# normal aov will not give error terms
model <- aov(Filtration ~ Temperature * Pressure * Formaldehyde * StirringRate,
             data=Table6.10)
summary(model)

# create a normal probability plot based on the effect
df <- Table6.10
colnames(df) <- c('A', 'B', 'C', 'D', 'Filtration', 'Block')
X <- model.matrix(~ -1 + A*B*C*D, data=df)
effects <- apply(X, 2, function(w){ sum((w*df$Filtration) / (0.5 * dim(df)[1])) })
names(effects) <- colnames(X)
effects

# normal plot
locs <- qqnorm(effects, main='Normal Probability Plot')
qqline(effects, col='orange')
ix <- c(1,3,4,6,8)
text(x=locs$x[ix], y=locs$y[ix], labels=names(effects[ix]), pos=c(2, 4, 4, 4, 4))

# half normal plot
library(DoE.base)
Fo <- Table6.10$Formaldehyde
Te <- Table6.10$Temperature
St <- Table6.10$StirringRate
Pr <- Table6.10$Pressure
Fi <- Table6.10$Filtration
model <- aov(Fi ~ Te * Pr * Fo * St, data=Table6.10)
vals <- halfnormal(model, alpha=0.05)
print(vals$signif)


# Data Transformation in a Factorial Design -------------------------------
library(DoE.base)
head(Example6.3)
# drill rate with other four factors
model <- lm(DrillRate ~ .*.*.*., data=Example6.3)
vals <- halfnormal(model)
print(vals$signif) # three effects are significant

# ignore factor 'load'
model2 <- aov(DrillRate ~ Mud + RotationalSpeed + FlowRate +
              FlowRate:Mud + FlowRate:RotationalSpeed,
              data=Example6.3)
summary(model2)

# check residues
plot(predict(model2), resid(model2)) # show unequal variance

# transform function - log on drill rate
df <- Example6.3
df[,'DrillRate'] <- log(df[,'DrillRate'])
model3 <- lm(DrillRate ~ .*.*.*., data=df)
vals <- halfnormal(model3)

print(vals$signif) # still same three effects show significance
model4 <- lm(DrillRate ~ FlowRate * RotationalSpeed * Mud, data=df)
anova(model4)

# fit the main-effects-only model and check for normality of the residuals
model4 <- lm(DrillRate ~ FlowRate + RotationalSpeed + Mud, data=df)
qqnorm(resid(model4))
qqline(resid(model4), col='orange')

# check residues
plot(predict(model4), resid(model4)) # show unequal variance


# — Location and Dispersion Effects in an Unreplicated Factorial ----------
# how to reduce the number of defects
library(DoE.base)
head(Example6.4)
model <- lm(Defects ~ .*.*.*., data=Example6.4)
vals <- halfnormal(model) # Temperature and ResinFlow significant

model2 <- lm(Defects ~ Temperature * ResinFlow, data=Example6.4)
anova(model2) # interaction not significant

# include main effects only
model2 <- lm(Defects ~ Temperature + ResinFlow, data=Example6.4)

# check clamp time vs residues
plot(Example6.4$ClampTime, resid(model2)) # equal variance not true

ix <- Example6.4$ClampTime == 1   # not used?
X <- model.matrix(Defects ~ -1 + . * . * . * ., data=Example6.4)
Fi.star <- apply(
  X,
  2,
  function(w){
    log(
      sd(resid(model2)[w == 1])^2
      /
        sd(resid(model2)[w != 1])^2
    )
  }
)
vals <- halfnormal(Fi.star)


