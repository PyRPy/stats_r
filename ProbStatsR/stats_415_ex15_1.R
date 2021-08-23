
# 15.1 - A Test for the Slope ---------------------------------------------

# alligator length and weight
logL = c(3.87, 3.61, 4.33, 3.43, 3.81, 
         3.83, 3.46, 3.76, 3.50, 3.58,
         4.19, 3.78, 3.71, 3.73, 3.78)

logW = c(4.87, 3.93, 6.46, 3.33, 4.38,
         4.70, 3.50, 4.50, 3.58, 3.64,
         5.90, 4.43, 4.38, 4.42, 4.25)

linearReg = lm(logW ~ logL)
summary(linearReg)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -8.4761     0.5007  -16.93 3.08e-10 ***
# logL          3.4311     0.1330   25.80 1.49e-12 ***

aov(linearReg)
