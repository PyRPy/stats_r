
# 17. POSTSTRATIFICATION AND MISSING-DATA IMPUTATION ----------------------
# https://avehtari.github.io/ROS-Examples/Poststrat/poststrat2.html
# https://online.stat.psu.edu/stat506/lesson/6/6.3
# https://avehtari.github.io/ROS-Examples/Imputation/imputation.html
# Load packages
library("rstanarm")
invlogit <- plogis

# create fake data
# Create an empty poststratification table,
# with rows for each of the 2×4×4 sorts of people.

J <- c(2, 4, 4)
poststrat <- as.data.frame(array(NA, c(prod(J), length(J)+1)))
colnames(poststrat) <- c("sex", "age", "eth", "N")
count <- 0
for (i1 in 1:J[1]){
  for (i2 in 1:J[2]){
    for (i3 in 1:J[3]){
      count <- count + 1
      poststrat[count, 1:3] <- c(i1, i2, i3)
    }
  }
}

# make up numbers for the populations of the cells
p_sex <- c(0.52, 0.48)
p_age <- c(0.2, 0.25, 0.3, 0.25)
p_eth <- c(0.7, 0.1, 0.1, 0.1)
for (j in 1:prod(J)){
  poststrat$N[j] <- 250e6 * p_sex[poststrat[j,1]] * p_age[poststrat[j,2]] *
    p_eth[poststrat[j,3]]
}

# Hypothesize a nonresponse pattern in which women, older people,
# and whites are more likely to respond than men, younger people,
# and minorities

p_response_baseline <- 0.1
p_response_sex <- c(1, 0.8)
p_response_age <- c(1, 1.2, 1.6, 2.5)
p_response_eth <- c(1, 0.8, 0.7, 0.6)
p_response <- rep(NA, prod(J))
for (j in 1:prod(J)){
  p_response[j] <- p_response_baseline * p_response_sex[poststrat[j,1]] *
    p_response_age[poststrat[j,2]] * p_response_eth[poststrat[j,3]]
}

# Sample from the assumed population with the assumed
# nonresponse probabilities
n <- 1000
people <- sample(prod(J), n, replace=TRUE, prob=poststrat$N*p_response)
# For respondent i, people[i] is that person's poststrat cell,
# some number between 1 and 32
n_cell <- rep(NA, prod(J))
for (j in 1:prod(J)){
  n_cell[j] <- sum(people==j)
}
print(cbind(poststrat, n_cell/n, poststrat$N/sum(poststrat$N)))

# Assume the survey responses come from a logistic regression
# with these coefficients
coef_intercept <- 0.6
coef_sex <- c(0, -0.2)
coef_age <- c(0, -0.2, -0.3, -0.4)
coef_eth <- c(0, 0.6, 0.3, 0.3)

# The probabilities are:
prob_yes <- rep(NA, prod(J))
for (j in 1:prod(J)){
  prob_yes[j] <- invlogit(coef_intercept + coef_sex[poststrat[j,1]] +
                            coef_age[poststrat[j,2]] + coef_eth[poststrat[j,3]])
}

## ------------Simulate the fake data ----------------- ##
y <- rbinom(n, 1, prob_yes[people])

# Linear model
sex <- poststrat[people,1]
age <- poststrat[people,2]
eth <- poststrat[people,3]
fake <- data.frame(y, sex, age, eth)
fit <- stan_glm(y ~ factor(sex) + factor(age) + factor(eth),
                family=binomial(link="logit"), data=fake, refresh=0)
print(fit)

# Prediction
pred_sim <- posterior_epred(fit, newdata=as.data.frame(poststrat))
pred_est <- colMeans(pred_sim)
round(cbind(poststrat, prob_yes, pred_est), 2)

## ------ Poststratification ------ ##
poststrat_est <- sum(poststrat$N*pred_est)/sum(poststrat$N)
round(poststrat_est, 2)

# plus uncertainty
poststrat_sim <- pred_sim %*% poststrat$N / sum(poststrat$N)
round(c(mean(poststrat_sim), sd(poststrat_sim)), 3)



# MISSING-DATA IMPUTATION -------------------------------------------------
library("rstanarm")

# data
SIS <- read.csv("ROS_Data/SIS.csv")
head(SIS)

# helper functions for imputation
impute <- function(a, a_impute) {
  ifelse(is.na(a), a_impute, a)
}

topcode <- function(a, top) {
  ifelse(a > top, top, a)
}

# Deterministic imputation
# Impute 0 earnings using the logical rule (if worked 0 months and 0 hrs/wk)
SIS$earnings_top <- topcode(SIS$earnings, 100)
SIS$earnings_top[SIS$workhrs_top==0 & SIS$workmos==0] <- 0

# Create a dataset with all predictor variables
n <- nrow(SIS)
SIS_predictors <- SIS[,c("male","over65","white","immig","educ_r","workmos",
                         "workhrs_top","any_ssi","any_welfare","any_charity")]

# Impute subset of earnings that are nonzero: linear scale
fit_imp_1 <- stan_glm(
  earnings ~ male + over65 + white + immig + educ_r +
    workmos + workhrs_top + any_ssi +
    any_welfare + any_charity,
  data = SIS,
  subset = earnings > 0,
  refresh = 0
)
print(fit_imp_1)

# point predictions
pred_1 <- colMeans(posterior_linpred(fit_imp_1, newdata = SIS_predictors))
SIS$earnings_imp_1 <- impute(SIS$earnings, pred_1)

# Impute subset of earnings that are nonzero: square root scale and topcoding
fit_imp_2 <- stan_glm(
  sqrt(earnings_top) ~ male + over65 + white + immig +
    educ_r + workmos + workhrs_top + any_ssi +
    any_welfare + any_charity,
  data = SIS,
  subset = earnings > 0,
  refresh = 0
)
print(fit_imp_2)

# point predictions
pred_2_sqrt <- colMeans(posterior_linpred(fit_imp_2, newdata = SIS_predictors))
pred_2 <- topcode(pred_2_sqrt^2, 100)
SIS$earnings_imp_2 <- impute(SIS$earnings_top, pred_2)

# One random imputation
# Linear scale (use fitted model fit_imp_1)
pred_3 <- posterior_predict(fit_imp_1, newdata = SIS_predictors, draws = 1)
SIS$earnings_imp_3 <- impute(SIS$earnings, pred_3)

# Square root scale and topcoding (use fitted model fit_imp_2)
pred_4_sqrt <- posterior_predict(fit_imp_2, newdata = SIS_predictors, draws = 1)
pred_4 <- topcode(pred_4_sqrt^2, 100)
SIS$earnings_imp_4 <- impute(SIS$earnings_top, pred_4)

# Iterative regression imputation
# Starting values
random_imp <- function (a){
  missing <- is.na(a)
  n_missing <- sum(missing)
  a_obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a_obs, n_missing)
  imputed
}
SIS$interest_imp <- random_imp(SIS$interest)
SIS$earnings_imp <- random_imp(SIS$earnings)

# Simplest regression imputation
n_loop <- 10
for (s in 1:n_loop){
  fit <- stan_glm(earnings ~ interest_imp + male + over65 + white +
                    immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare +
                    any_charity, data=SIS, refresh = 0)
  SIS_predictors <- SIS[,c("male","over65","white","immig","educ_r","workmos",
                           "workhrs_top","any_ssi","any_welfare","any_charity",
                           "interest_imp", "earnings_imp")]
  pred1 <- posterior_predict(fit, newdata = SIS_predictors, draws = 1)
  SIS$earnings_imp <- impute(SIS$earnings, pred1)

  fit <- stan_glm(interest ~ earnings_imp + male + over65 + white +
                    immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare +
                    any_charity, data=SIS, refresh = 0)
  SIS_predictors <- SIS[,c("male","over65","white","immig","educ_r","workmos",
                           "workhrs_top","any_ssi","any_welfare","any_charity",
                           "interest_imp", "earnings_imp")]
  pred2 <- posterior_predict(fit, newdata = SIS_predictors, draws = 1)
  SIS$interest_imp <- impute(SIS$interest, pred2)
}
head(SIS)
