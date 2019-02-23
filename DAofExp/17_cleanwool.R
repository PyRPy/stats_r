# cleanwool.r, clean wool experiment, Section 17.11.1, pp660-1,
# including code from text p660 and Table 17.15 p661

wool.data <- read.table("clean.wool.txt", header=T) 
head(wool.data, 3)
model1 <- aov(y ~ factor(bale), data=wool.data)  
summary(model1)

# Compute predicted values, residuals, standardized residuals
wool.data <- within(wool.data, {
   ypred = fitted(model1); e = resid(model1); z = e/sd(e);
   # Compute Blom's normal scores
   n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25)) })

print(head(wool.data, 3), digits=4)

# Generate residual plots
plot(z ~ bale, data=wool.data, ylab="Standardized Residuals", las=1)
  abline(h=0)  # Horizontal line at zero

# Code from text p660
# Plot z ~ ypred using bale as plotting symbol
plot(z ~ ypred, data=wool.data, ylab="Standardized Residuals", las=1, 
     type="n") # Suppress plotting of circles
  text(z ~ ypred, bale, cex=0.75, data=wool.data) # Plot bale number
  mtext("Plotting symbol is bale", side=3, adj=1, line=1) # Margin text
  abline(h=0)

plot(z ~ nscore, data=wool.data, ylab="Standardized Residuals", las=1)
  qqline(wool.data$z)  # Line through 1st and 3rd quantile points

# Residual plots without the outlier
wool2.data <- wool.data[-c(3),] 
head(wool2.data, 3)
model2 <- aov(y ~ factor(bale), data=wool2.data)  
summary(model2)

# Compute predicted values, residuals, standardized residuals
wool2.data <- within(wool2.data, {
   ypred = fitted(model2); e = resid(model2); z = e/sd(e);
   # Compute Blom's normal scores
   n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25)) })
print(head(wool2.data, 3), digits=4)

# Generate residual plots
plot(z ~ bale, data=wool2.data, ylab="Standardized Residuals", las=1)
  abline(h=0)  # Horizontal line at zero
# Plot z ~ ypred using bale as plotting symbol (adapted from ch6Rcode.txt)
plot(z ~ ypred, data=wool2.data, ylab="Standardized Residuals", 
     las=1, type="n") # Suppress plotting of circles
  text(z ~ ypred, bale, cex=0.75, data=wool2.data) # Plot bale labels
  mtext("Plotting symbol is bale", side=3, adj=1, line=1) # Margin text
  abline(h=0)
plot(z ~ nscore, data=wool2.data, ylab="Standardized Residuals", las=1)
  qqline(wool2.data$z)  # Line through 1st and 3rd quantile points

# Table 17.15, p661
# Plot standardized treatment means versus their normal scores
AvgContent <- by(wool.data$y, wool.data$bale, mean)  # Col of means (by bale)
# Standarized treatment means
StdzdAvg <- (AvgContent - mean(AvgContent))/sd(AvgContent - mean(AvgContent))

# Normal scores
nscore <- qqnorm(StdzdAvg)$x
plot(StdzdAvg ~ nscore, ylab="Standardized Trtmt Means", las=1)
  qqline(StdzdAvg)  # Line through 1st and 3rd quantile points