
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
