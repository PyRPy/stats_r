# sludge.r, sludge experiment, Tables 15.45-6, pp544-5,
# and text p545 comments

# Table 15.45, p544
sludge.data <- read.table("Data/sludge.txt", header=T) # Read A:E,y

# Create variable TC within sludge.data:
sludge.data <- within(sludge.data, {TC = 10000*A+1000*B+100*C+10*D+E})

# Create factor variables within sludge.data:
sludge.data <- within(sludge.data, 
   {fA = factor(A); fB = factor(B); fC = factor(C); 
     fD = factor(D);  fE = factor(E); fTC = factor(TC)}) 


head(sludge.data, 3)

# Analysis of variance: cell means model
modelTC <- lm(y ~ fTC, data=sludge.data)
anova(modelTC)

# Contrast estimates: cell means model
library(lsmeans)
lsmTC <- lsmeans(modelTC, ~ fTC)
contrast(lsmTC, list(A=c(-1,-1,-1,-1, 1, 1, 1, 1)/4,
                     B=c(-1,-1, 1, 1,-1,-1, 1, 1)/4,
                     C=c(-1, 1,-1, 1,-1, 1,-1, 1)/4,
                     D=c( 1, 1,-1,-1,-1,-1, 1, 1)/4,
                     E=c(-1, 1, 1,-1, 1,-1,-1, 1)/4,
                    AC=c( 1,-1, 1,-1,-1, 1,-1, 1)/4,
                    BC=c( 1,-1,-1, 1, 1,-1,-1, 1)/4))

# Table 15.46, p545
# Analysis of variance: factorial effects model
modelFE <- lm(y ~ fA+fB+fC+fD+fE+fA:fC+fB:fC, data=sludge.data)
anova(modelFE)

# Contrast estimates: factorial effects model
lsmA <- lsmeans(modelFE, ~ fA); contrast(lsmA, list(A=c(-1,1)))
lsmB <- lsmeans(modelFE, ~ fB); contrast(lsmB, list(B=c(-1,1)))
lsmC <- lsmeans(modelFE, ~ fC); contrast(lsmC, list(C=c(-1,1)))
lsmD <- lsmeans(modelFE, ~ fD); contrast(lsmD, list(D=c(-1,1)))
lsmE <- lsmeans(modelFE, ~ fE); contrast(lsmE, list(E=c(-1,1)))
lsmAC <- lsmeans(modelFE, ~ fA:fC); contrast(lsmAC, list(AC=c(1,-1,-1,1)/2))
lsmBC <- lsmeans(modelFE, ~ fB:fC); contrast(lsmBC, list(BC=c(1,-1,-1,1)/2))

# Illustrate consequences of an extra term in the model, text p545 comments
model2FE <- lm(y ~ fA+fB+fC+fD+fE+fA:fC+fB:fC+fB:fD, data=sludge.data)
anova(model2FE)
drop1(model2FE, ~., test = "F") 