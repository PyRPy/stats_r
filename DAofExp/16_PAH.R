# PAH.r, PAH recovery experiment, Tables 16.19-20, pp604-5,
# and code from text p605

# Variation on Table 16.19, p604
# Read and code data, creating factor variable for blocks
pah.data <- read.table("Data/pah.txt", header=T)
pah.data$fBlock = factor(pah.data$Block)

library(rsm)
# Using SAS coding:
pah.ccd.data <- coded.data(pah.data, zP ~ (Pres - 200)/100, 
  zT ~ (Temp - 70)/30, zET ~ (ET - 35)/25, zMC ~ (MC - 10)/10 )
head(pah.ccd.data, 3)

# Table 16.20, p605
# Second-order model and analysis
model2 <- rsm(y ~ fBlock + SO(zP, zT, zET, zMC), data=pah.ccd.data)
summary(model2)

# Code from text p605
# Contour plots
par(mfrow = c(3, 2))
contour(model2, ~ zP + zT + zET + zMC, at=round(xs(model2),3), image=T, las=1)

