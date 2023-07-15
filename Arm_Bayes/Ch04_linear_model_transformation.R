
## Read & clean the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# 4.1 Linear transformations ----------------------------------------------


library ("arm")
library(haven) # read .dta files
heights <- read_dta ("heights.dta")
attach(heights)

# recode sex variable
male <- 2 - sex

# (for simplicity) remove cases with missing data
ok <- !is.na (earn+height+sex) & earn>0
heights.clean <- as.data.frame (cbind (earn, height, male)[ok,])
n <- nrow (heights.clean)

attach(heights.clean)
height.jitter.add <- runif (n, -.2, .2)

## Model fit
lm.earn <- lm (earn ~ height)
display (lm.earn)
sim.earn <- sim (lm.earn)
beta.hat <- coef(lm.earn)


# 4.2 Centering and standardizing, ----------------------------------------

kidiq <- read_dta("ARM_Data/child.iq/kidiq.dta")
attach(kidiq)

## Estimations

# original model
fit.4 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq)
summary(fit.4)

# centering by subtracting the mean
c_mom_hs <- mom_hs - mean(mom_hs)
c_mom_iq <- mom_iq - mean(mom_iq)

fit.5 <- lm (kid_score ~ c_mom_hs + c_mom_iq + c_mom_hs:c_mom_iq)
summary(fit.5)

# using a conventional centering point
c2_mom_hs <- mom_hs - 0.5
c2_mom_iq <- mom_iq - 100

fit.6 <- lm (kid_score ~ c2_mom_hs + c2_mom_iq + c2_mom_hs:c2_mom_iq)
display(fit.6)

# centering by subtracting the mean & dividing by 2 sd
z_mom_hs <- (mom_hs - mean(mom_hs))/(2*sd(mom_hs))
z_mom_iq <- (mom_iq - mean(mom_iq))/(2*sd(mom_iq))

fit.7 <- lm (kid_score ~ z_mom_hs + z_mom_iq + z_mom_hs:z_mom_iq)
summary(fit.7)
# no improvement on R-square, what's the point in transforming.


# 4.4 Logarithmic transformations -----------------------------------------

log.earn <- log(earn)
earn.logmodel.1 <- lm(log.earn ~ height)
display(earn.logmodel.1)

## Log-base-10 transformation

log10.earn <- log10(earn)
earn.log10model <- lm(log10.earn ~ height)
display(earn.log10model)

## Log scale regression model

earn.logmodel.2 <- lm(log.earn ~ height + male[ok]) # fix length problem
display(earn.logmodel.2)

## Including interactions

earn.logmodel.3 <- lm(log.earn ~ height + male[ok] + height:male[ok])
display(earn.logmodel.3)

## Linear transformations

z.height <- (height - mean(height))/sd(height)
earn.logmodel.4 <- lm(log.earn ~ z.height + male[ok] + z.height:male[ok])
display(earn.logmodel.4)

## Log-log model

log.height <- log(height)
earn.logmodel.5 <- lm(log.earn ~ log.height + male[ok])
display(earn.logmodel.5)

