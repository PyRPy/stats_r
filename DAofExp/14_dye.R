# dye.r, dye experiment, Table 14.17, p490

dye2.data <- read.table("Data/dye1.txt", header=T)
head(dye2.data, 3)

# Create factor variables
dye2.data <- within(dye2.data, 
           {fBlk = factor(BLK); fA = factor(A); 
              fB = factor(B);   fC = factor(C) })

# Analysis of variance
model1 <- lm(Y ~ fBlk + fA*fB*fC, data=dye2.data)
anova(model1)

# ANOVA without block effects for comparison
model2 <- lm(Y ~ fA*fB*fC, data=dye2.data)
anova(model2)

