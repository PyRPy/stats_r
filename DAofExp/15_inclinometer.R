# inclinometer.r, inclinometer experiment, product array, Table 15.47, p546

# Read data from file. Header: A B C D E F G y1 y2 y3 y4 y5 y6 y7 y8
ipa.data <- read.table("Data/inclinometer.product.txt", header=T)

# Create factor variables 
ipa.data <- within(ipa.data, 
  {fA=factor(A); fB=factor(B); fC=factor(C); fD=factor(D); 
   fE=factor(E); fF=factor(F); fG=factor(G) })

# Compute Avy and LnVar of data at each design treatment combo
ipa.data <- within(ipa.data, {Avy = (y1+y2+y3+y4+y5+y6+y7+y8)/8
    LnVar = log( ((y1*y1+y2*y2+y3*y3+y4*y4+y5*y5+y6*y6+y7*y7+y8*y8) 
                                                     - 8*Avy^2)/7 ) })
# Remove variables y1:y8
ipa.data <- within(ipa.data, {remove(y1,y2,y3,y4,y5,y6,y7,y8)} )
head(ipa.data, 3)

# Analysis of log sample variance
model1 <- lm(LnVar ~ fA+fB+fC+fD+fE+fF+fG, data=ipa.data)
anova(model1)

# Least square means and contrast estimates for A (similarly for B--F) 
library(lsmeans)
lsmA1 <- lsmeans(model1, ~ fA)
lsmA1
contrast(lsmA1, list(Alin=c(-1,0,1), Aquad=c(1,-2,1)))

# Regression for log sample variance (to get contrast SS's)
model2 <- lm(LnVar ~ A+I(A^2)+B+I(B^2)+C+I(C^2)+D+I(D^2)
                      +E+I(E^2)+F+I(F^2)+G+I(G^2), data=ipa.data)
anova(model2)

# Analysis of sample mean
model3 <- lm(Avy ~ fA+fB+fC+fD+fE+fF+fG, data=ipa.data)
anova(model3)

# Least square means and contrast estimates for A (similarly for B--F) 
# library(lsmeans)
lsmA3 <- lsmeans(model3, ~ fA)
lsmA3
contrast(lsmA3, list(Alin=c(-1,0,1), Aquad=c(1,-2,1)))

# Regression for sample mean (to get contrast SS's, including for G)
model4 <- lm(Avy ~ A+I(A^2)+B+I(B^2)+C+I(C^2)+D+I(D^2)
                  +E+I(E^2)+F+I(F^2)+G+I(G^2), data=ipa.data)
anova(model4)