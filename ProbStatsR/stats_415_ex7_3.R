# 7.5 - Confidence Intervals for Regression Parameters --------------------

price <- c(190, 160, 134, 129, 172,
           197, 167, 239, 542, 372, 
           245, 376, 454, 410)

catch <- c(7.23, 8.53, 9.82, 10.26, 8.96,
           12.27, 10.28, 4.45, 1.78, 4.00,
           3.30, 4.30, 0.80, 0.50)

plot(catch, price)
linearReg <- lm(price ~ catch)
summary(linearReg)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -110.09  -38.34  -19.07   34.47  142.22 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  452.119     36.824  12.278 3.75e-08 ***
# catch        -29.402      5.091  -5.775 8.81e-05 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 71.69 on 12 degrees of freedom
# Multiple R-squared:  0.7354,	Adjusted R-squared:  0.7134 
# F-statistic: 33.36 on 1 and 12 DF,  p-value: 8.805e-05

# Find a 95% confidence interval for the slope
-29.402 + qt(c(0.025, 0.975), 12) * sqrt(71.69^2 / sum((catch - mean(catch))^2))
qt(c(0.025, 0.975), 12) * sqrt(71.69^2 / sum((catch - mean(catch))^2))

# Find a 95% confidence interval for the intercept
mean(price) + qt(c(0.025, 0.975), 12) * sqrt(71.69^2/length(catch))

# centered x
catch_centered <- catch - mean(catch)
plot(catch_centered, price)

linearReg2 <- lm(price ~ catch_centered)
summary(linearReg2)
