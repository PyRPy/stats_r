# reactiontime2.r, reaction time experiment, Section 6.9.3 plots, pp190-191

# Read data as in Table 6.15, page 185
react.data = read.table("Data/reaction.time.txt", header=T)
react.data = head(react.data, 14) # Keep first 14 observations

# Create trtmt combo vbl TC and factors fA and fB within data set
react.data = within(react.data,
                    {TC = 10*A + B; fA = factor(A); fB = factor(B)})

# Fit model
options(contrasts = c("contr.sum", "contr.poly"))
modelAB = aov(y ~ fA + fB + fA:fB, data = react.data)

# Compute predicted values, residuals, standardized residuals, normal scores
react.data = within(react.data, {
  # Compute predicted, residual, and standardized residual values
  ypred = fitted(modelAB); e = resid(modelAB); z = e/sd(e);
  # Compute Blom's normal scores
  n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25)) })
# Display first 3 lines of react.data, 4 digits per variable
print(head(react.data, 3), digits=4)

# Generate residual plots

# Plot of z versus A, distinguishing levels of B, p190
plot(z ~ A, data=react.data, xaxt="n", type="n") # Suppress x-axis, pts
  axis(1, at=seq(1,2,1)) # Add x-axis with tick marks from 1 to 2 by 1
  text(z ~ A, B, cex=0.75, data=react.data) # Plot z vs A using B label
  mtext("B=1,2,3", side=3, adj=1, line=1)
  abline(h=0) # Horizontal line at zero
  
# Interaction plot, p190  
interaction.plot(x.factor = react.data$fA, trace.factor = react.data$fB,
                 response = react.data$y, type ="b",
                 xlab ="A", trace.label ="B", ylab ="Mean of y")

# These 2 plots (p190) are not as good as the pair below.  
# plot(modelAB$res ~ react.data$TC, xlab ="AB", ylab ="Residual")
# plot(react.data$y ~ react.data$TC, xlab ="AB", ylab ="y")

# Plot residuas and y's versus TC with TC levels equally spaced, p191
plot(modelAB$res ~ react.data$Trtmt, xaxt="n", xlab="AB", ylab="Residual")
  axis(1, at = react.data$Trtmt, labels = react.data$fTC)
plot(react.data$y ~ react.data$Trtmt, xaxt="n", xlab="AB", ylab="y")
  axis(1, at = react.data$Trtmt, labels = react.data$fTC)

# Compute sample variance of residuals by level of A, p191
by(modelAB$res, react.data$A, var)

# Generate the usual residual plots as in chapter 5
plot(z ~ Trtmt, data=react.data, ylab="Standardized Residuals", las=1)
abline(h=0) # Horizontal line at zero
plot(z ~ Order, data=react.data, ylab="Standardized Residuals", las=1)
abline(h=0)
plot(z ~ ypred, data=react.data, ylab="Standardized Residuals", las=1)
abline(h=0)
plot(z ~ nscore, data=react.data, ylab="Standardized Residuals", las=1)
qqline(react.data$z) # Line through 1st and 3rd quantile points

# A simpler way to generate the normal probability plot
qqnorm(react.data$z); qqline(react.data$z)
