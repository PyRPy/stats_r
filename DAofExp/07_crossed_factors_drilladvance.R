# drilladvance.r, drill advance experiment, Table 7.12, p231
# page - 220
# Daniel (1976) described a single replicate 2×2×2×2 experiment to 
# study the effects of four treatment factors on the rate of advance 
# of a small stone drill. The treatment factors were “load on the drill” (A), 
# “flow rate through the drill” (B), “speed of rotation” (C), and “type of 
# mud used in drilling” (D). Each factor was observed at two levels, 
# coded 1 and 2.

# Input data for A, B, C, D and Advance
drill.data <- read.table("Data/drill.advance.txt", header=T)

# Compute log advance, and convert levels 1 and 2 to coeffs -1 and 1, resp.
drill.data <- within(drill.data,
  { y = log10(Advance); A = 2*A-3; B = 2*B-3; C = 2*C-3; D = 2*D-3 })

head(drill.data, 3)

# Fit regression model with interactions to obtain estimates
model1 <- lm(y ~ A*B*C*D, data=drill.data)
model1$coefficients # Show estimates

# Generate half-normal plot of effect estimates
# install.packages("gplots")
library(gplots)
qqnorm.aov(model1, xlab="Half-Normal Scores",
           ylab="Normalized Absolute Estimates")
