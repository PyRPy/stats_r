# mungbean.r, mung bean experiment, Table 5.11 (p 126) and text code (p129)

# Table 5.11, page 126
mung.data <- read.table("Data/mungbean8.txt", header=T) 
colnames(mung.data) <- c('Obs', 'Order', 'Trtmt', 'Length')
model1 <- aov(Length ~ factor(Trtmt), data=mung.data)  

# Compute predicted values, residuals, standardized residuals, normal scores
mung.data <- within(mung.data, {
   # Compute predicted, residual, and standardized residual values
   ypred = fitted(model1); e = resid(model1); z = e/sd(e); 
   # Compute Blom's normal scores
   n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25)) })
   # nscore = qqnorm(z)$x }); # Blom's for n<=10, else qnorm((q-0.5)/n)

# Display first 3 lines of mung.data, 4 digits per variable
print(head(mung.data, 3), digits=4)

# Generate residual plots
plot(z ~ Trtmt, data=mung.data, ylab="Standardized Residuals", las=1)
  abline(h=0)  # Horizontal line at zero
plot(z ~ Order, data=mung.data, ylab="Standardized Residuals", las=1)
  abline(h=0)  
plot(z ~ ypred, data=mung.data, ylab="Standardized Residuals", las=1) 
  abline(h=0)
plot(z ~ nscore, data=mung.data, ylab="Standardized Residuals", las=1)
  qqline(mung.data$z)  # Line through 1st and 3rd quantile points
# A simpler way to generate the normal probability plot
qqnorm(mung.data$z); qqline(mung.data$z)  

# R code from text page 129
# NPPlot of standardized residuals by treatment
by(mung.data$z, mung.data$Trtmt, qqnorm)
# Or, as illustrated here for treatment 1
qqnorm(mung.data$z[mung.data$Trtmt == 1], main = "Normal Q-Q Plot: Trtmt = 1") 
  qqline(mung.data$z)  # Line through 1st and 3rd quantile points

# plot the model directly
par(mfrow=c(2,2))
plot(model1)
