
# Six prototype models; fitting in R --------------------------------------
# pseudo code for reference

# Varying-intercept linear regression
m1 <- lmer(y ~ x + (1 | group))

# with a group-level predictor u
u.full <- u[group]
m2 <- lmer(y ~ x + u.full + (1|group))

# with varying intercepts and varying slopes
m3 <- lmer(y ~ x + u.full + x:u.full + (1+x | group))

# with binary data and logistic regression
m4 <- lmer(y ~ x + (1|group), family = binomial(link="logit"))

# with count data and overdispersed Poisson regression with offset log(z)
m5 <- log.z <- log(z)
lmer(y ~ x + (1 | group), offset=log.z, family=quasipoisson(link="log"))

# two-way data structure with replication
state.occupation <- max(occupation) * (state -1) + occupation
m6 <- lmer(y ~ 1 + (1|state) + (1 | occupation) + (1|state.occupation))
