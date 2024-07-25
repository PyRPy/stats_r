
# Chapter12 Robust Parameter Design ---------------------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)

# filtration example, with temperature difficult to control
head(Table6.10) # coded varaibles
df <- Table6.10
colnames(df) <- c('z', 'x1', 'x2', 'x3', 'y', 'Block')
model <- lm(y ~ z * (x1 + x2 + x3), data=df)
print(summary(model)) # x1 is not significant

# run another model
model <- lm(y ~ z * (x2 + x3), data=df)
print(summary(model))

# overlay two contours
x2 <- seq(from=-1, to=1, length.out=100)
x3 <- seq(from=-1, to=1, length.out=100)
z <- matrix(predict(model, cbind(
  'z'=rep(0, 100^2),
  expand.grid('x2'=x2, 'x3'=x3)
)), ncol=100)

poe.coefs <- (
  model.matrix(
    ~ z * (x2 + x3),
    data=cbind(
      'z'=rep(1, 100^2),
      expand.grid('x2'=x2, 'x3'=x3)
    )
  )
  %*%
    matrix(c(0, coef(model)['z'], 0, 0,
             coef(model)[c('z:x2','z:x3')]))
)^2

z.poe <- matrix(sqrt(poe.coefs), nrow=100)

contour(x2, x3, z, col='blue', xlab='Concentration',
        ylab='StirringRate')
contour(x2, x3, z.poe, col='orange', add=TRUE)
