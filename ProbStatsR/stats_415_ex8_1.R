
# 8.1 - A Confidence Interval for the Mean of Y ---------------------------

dat <- read.delim("OldFaithful.TXT")
head(dat)
with(dat, plot(DURATION, NEXT))

linearReg <- lm(NEXT ~ DURATION, data = dat)
summary(linearReg)

yhat = 33.828 + 10.741 * 4.8 # x = 4.8
t_inter = qt(c(0.025, 1-0.025), nrow(dat)-2)
CI = yhat + t_inter * 6.683 * sqrt(1/nrow(dat) + 
     (4.8 - mean(dat$DURATION))^2/sum((dat$DURATION-mean(dat$DURATION))^2))
round(CI, 3)

# use R base
predict(linearReg, newdata = data.frame(DURATION = 4.8), interval = "confidence")

# 8.2 - A Prediction Interval for a New Y ---------------------------------

PI = yhat + t_inter * 6.683 * sqrt(1 + 1/nrow(dat) + 
     (4.8 - mean(dat$DURATION))^2/sum((dat$DURATION-mean(dat$DURATION))^2))
round(PI, 3)

# use R base
predict(linearReg, newdata = data.frame(DURATION = 4.8), interval = "prediction")
