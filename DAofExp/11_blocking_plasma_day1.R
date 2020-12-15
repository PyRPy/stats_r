# plasma-day1.r, plasma experiment, Tables 11.25-6, pp388-9

# Table 11.25, p388
plasma.data <- read.table("Data/plasma.txt", header=T)
plasma.data <- within(plasma.data, 
  {fBlock = factor(Block); fTrtmt = factor(Trtmt) })

head(plasma.data, 3)

# Analysis of day 1 data
day1.data <- subset(plasma.data, Day==1) # Keep data with "Day==1"
model2 <- lm(y ~ fBlock + fTrtmt, data=day1.data)
summary(model2) # anova different than linear regression format
anova(model2)
drop1(model2, ~., test="F")

# Scheffe's method
library(lsmeans)
lsmTrtmt <- lsmeans(model2, ~ fTrtmt)
lsmTrtmt
summary(contrast(lsmTrtmt, method="pairwise", adjust="Scheffe"),
        infer=c(T,T))


# Additional code from Table 11.26, p389
# Plotting day 1 data adjusted for block effects

# Create column of block effect estimates, and compute the avg estimate
model2$coefficients # Display all model coefficient estimates
thetahat <- c(0, model2$coefficients[2:3]) # Create col of block effect ests
avgest <- mean(thetahat) # Compute average block effect estimate

# Compute adjusted y-values: yadj = y - (thetahat.h - thetahat.bar)
day1.data$yadj <- day1.data$y - (thetahat[day1.data$Block] - avgest)

# Plot day 1 data adjusted for block effects
plot(yadj ~ Trtmt, data=day1.data, xlab="Treatment", ylab="y Adjusted")
