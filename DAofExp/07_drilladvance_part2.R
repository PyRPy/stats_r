# drilladvance2.r, drill advance experiment, Table 7.13, p233

# Input data for A, B, C, D and Advance
drill.data <- read.table("Data/drill.advance.txt", header=T)

# Compute log advance, and convert levels 1 and 2 to coeffs -1 and 1, resp.
drill.data <- within(drill.data,
  { y = log10(Advance); A = 2*A-3; B = 2*B-3; C = 2*C-3; D = 2*D-3 })

# Fit regression model with interactions to get estimates and SSs
model1 <- lm(y ~ A*B*C*D, data=drill.data)

# Save estimates, scaled to be difference of 2 averages
estimate <- 2*(model1$coefficients[2:16])
estimate

# Save sums of squares for effects
SS <- anova(model1)[1:15,2]
SS

# Order estimates and SSs in deceasing magnitude
estimate <- estimate[rev(order(SS))]
SS <- SS[rev(order(SS))]

# For each effect, compute msQ, stde, msd, and CIs
msQ <- numeric(15) # A column with 15 cells
sse <- sum(SS[8:15]) # sse = SS(8)+...+SS(15), a scalar

for(i in 1:7){msQ[i]=sse/8} # Compute msQ[1]--msQ[7]
for(i in 8:15){msQ[i] = (sse - SS[i] + SS[7])/8} # msQ[8]--msQ[15]

stde <- sqrt(msQ/4)
msd = 9.04*stde
LCL = estimate - msd
UCL = estimate + msd # CIs

# Display results to 5 decimal places
round( cbind(estimate, SS, msQ, stde, msd, LCL, UCL), digits=5)

