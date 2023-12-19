
# Linear regression with multiple predictors ------------------------------
# https://avehtari.github.io/ROS-Examples/examples.html
# https://avehtari.github.io/ROS-Examples/NES/nes_linear.html
# Load packages

library("rstanarm")
library("ggplot2")
library("bayesplot")
library("foreign")

# Load children's test scores data
kidiq <- read.csv("ROS_Data/kidiq.csv")
head(kidiq)

# Starting with a binary predictor
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq, refresh = 0)
print(fit_1)

# A single continuous predictor
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq, refresh = 0)
print(fit_2)

# plot the regression line with data
plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score",
     ylab="Child test score")
abline(coef(fit_2))

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(fit_2)[1], slope = coef(fit_2)[2]) +
  labs(x = "Mother IQ score", y = "Child test score")

# Including both predictors
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq, refresh = 0)
print(fit_3)
summary(fit_3)

# Two fitted regression lines -- model with interaction
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq,
                  data=kidiq,
                  refresh = 0)
print(fit_4)

# Indicator variables
# data
earnings <- read.csv("ROS_Data/earnings.csv")
head(earnings)

# Predict weight (in pounds) from height (in inches)
fit_1 <- stan_glm(weight ~ height, data=earnings, refresh = 0)
print(fit_1)

# Predict weight for 66 inches person
coefs_1 <- coef(fit_1)
predicted_1 <- coefs_1[1] + coefs_1[2]*66
round(predicted_1, 1)

new <- data.frame(height=66)
pred <- posterior_predict(fit_1, newdata=new)
cat("Predicted weight for a 66-inch-tall person is", round(mean(pred)),
    "pounds with a sd of", round(sd(pred)), "\n")

# Center heights
earnings$c_height <- earnings$height - 66
fit_2 <- stan_glm(weight ~ c_height, data=earnings, refresh = 0)
print(fit_2)

# Point prediction
new <- data.frame(c_height=4.0)
point_pred_2 <- predict(fit_2, newdata=new)
round(point_pred_2, 1)

# Posterior simulations
# variation coming from posterior uncertainty in the coefficients
linpred_2 <- posterior_linpred(fit_2, newdata=new)
hist(linpred_2)
quantile(linpred_2, c(0.025, 0.975)) # (171.4, 175.0)

# Posterior predictive simulations
# variation coming from posterior uncertainty in
# the coefficients and predictive uncertainty
postpred_2 <- posterior_predict(fit_2, newdata=new)
hist(postpred_2)
quantile(postpred_2, c(0.025, 0.975)) # (117.4, 229.8)

# Predict weight (in pounds) from height (in inches)
new <- data.frame(height=66)
pred <- posterior_predict(fit_1, newdata=new)
cat("Predicted weight for a 66-inch-tall person is",
    round(mean(pred)),
    "pounds with a sd of",
    round(sd(pred)), "\n")
# Including a binary variable in a regression
fit_3 <- stan_glm(weight ~ c_height + male, data=earnings, refresh = 0)
print(fit_3)

# Using indicator variables for multiple levels of a categorical predictor
fit_4 <- stan_glm(weight ~ c_height + male + factor(ethnicity),
                  data=earnings, refresh = 0)
print(fit_4)

# Choose the baseline category by setting the levels
earnings$eth <- factor(earnings$ethnicity,
                       levels=c("White", "Black", "Hispanic", "Other"))
fit_5 <- stan_glm(weight ~ c_height + male + ethnicity, data=earnings, refresh = 0)
print(fit_5)

# Alternatively create indicators for the four ethnic groups directly:
earnings$eth_white <- ifelse(earnings$ethnicity=="White", 1, 0)
earnings$eth_black <- ifelse(earnings$ethnicity=="Black", 1, 0)
earnings$eth_hispanic <- ifelse(earnings$ethnicity=="Hispanic", 1, 0)
earnings$eth_other <- ifelse(earnings$ethnicity=="Other", 1, 0)
fit_6 <- stan_glm(weight ~ c_height + male + eth_black + eth_hispanic + eth_other,
                  data=earnings, refresh = 0)
print(fit_6)

# Fitting the same regression to many datasets - National election study

data <- read.table("ROS_Data/nes.txt")
head(data)

# Partyid model to illustrate repeated model use (secret weapon)
regress_year <- function (yr) {
  this_year <- data[data$year==yr,]
  fit <- stan_glm(partyid7 ~ real_ideo + race_adj + factor(age_discrete) +
                    educ1 + female + income,
                  data=this_year, warmup = 500, iter = 1500, refresh = 0,
                  save_warmup = FALSE, cores = 1, open_progress = FALSE)
  coefs <- cbind(coef(fit),se(fit))
}

summary <- array (NA, c(9,2,8))

for (yr in seq(1972,2000,4)){
  i <- (yr-1968)/4
  summary[,,i] <- regress_year(yr)
}
yrs <- seq(1972,2000,4)

# Plot
coef_names <- c("Intercept", "Ideology", "Black", "Age_30_44",
                "Age_45_64", "Age_65_up", "Education", "Female", "Income")
par(mfrow=c(2,5), mar=c(2,3,2,2), tck=-.02, mgp=c(2,.7,0))
for (k in 1:9){
  plot(range(yrs), range(0,summary[k,1,]+.67*summary[k,2,],summary[k,1,]-.67*summary[k,2,]),
       type="n",
       xlab="",
       ylab="Coefficient",
       main=coef_names[k],
       mgp=c(1.2,.2,0),
       cex.main=1, cex.axis=1, cex.lab=1,
       tcl=-.1, bty="l", xaxt="n")
  axis(1, c(1972,1986,2000), mgp=c(.5,.3,0))
  abline(0,0, lty=2)
  points(yrs, summary[k,1,], pch=20)
  segments(yrs, summary[k,1,]-.67*summary[k,2,], yrs, summary[k,1,]+.67*summary[k,2,])
}
