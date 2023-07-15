
# Read in the data
library("arm")


# 6.2 Poisson regression, exposure, and overdispersion --------------------
data(arrests) # data not available, something not consistent.

wells <- read.table ("ARM_Data/wells.dat")
attach(wells)



# 6.4 Probit regression: normally distributed latent data -----------------
## Probit or logit
dist100 <- dist/100
fit.1 <- glm (switch ~ dist100, family=binomial(link="probit"))
display(fit.1)

# Figure 6.2
curve(dnorm(x), from=-6, to=6)

# other sections without data
# 6.7 Building more complex generalized linear models ---------------------

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
detach(heights.clean)
## Mixed discrete/continuous data
# not converged, error
heights.clean$earn.pos <- ifelse (heights.clean$earn>0, 1, 0)
fit.1a <- glm (earn.pos ~ height + male,
               family=binomial(link="logit"),
               data = heights.clean)
display(fit.1a)

log.earn <- log(earn)
fit.1b <- lm (log.earn ~ height + male, subset=earn>0)
display(fit.1b)

# 6.8 Constructive choice models ------------------------------------------
## Probit or logit
# data loaded in section 6.2
fit.1 <- glm (switch ~ dist100, family=binomial(link="logit"))
display(fit.1)
