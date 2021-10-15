
# Chapter 11 Generalized Linear Models ------------------------------------
library(tidyverse)
library(brms)
library(bayesplot)

# https://bookdown.org/marklhc/notes_bookdown/generalized-linear-models.html
# how common it was for researchers to report marginal  p-values

# readxl package can be used to import excel files
marginalp <- readxl::read_excel("marginals psych science revision_corrections.xlsx")

# Recode `Field` into a factor
marginalp <- marginalp %>% 
  # Filter out studies without any experiments
  filter(`Number of Experiments` >= 1) %>% 
  mutate(Field = factor(Field, 
                        labels = c("Cognitive Psychology", 
                                   "Developmental Psychology", 
                                   "Social Psychology"))) %>% 
  # Rename the outcome
  rename(marginal_p = `Marginals Yes/No`)

# Proportion of marginal p in each subfield across Years
marginalp %>% 
  ggplot(aes(x = Year, y = marginal_p)) + 
  stat_summary(aes(fill = Field), geom = "ribbon", alpha = 0.3) + 
  stat_summary(aes(col = Field), geom = "line") + 
  stat_summary(aes(col = Field), geom = "point") + 
  coord_cartesian(ylim = c(0, 0.7)) + 
  facet_wrap(~ Field)

head(marginalp)

# recode year10
marginalp$Year10[marginalp$Year == 1970] = 0
marginalp$Year10[marginalp$Year == 1980] = 1
marginalp$Year10[marginalp$Year == 1990] = 2
marginalp$Year10[marginalp$Year == 2000] = 3
marginalp$Year10[marginalp$Year == 2010] = 4


# fit a normal linear model
m1_norm <- brm(marginal_p ~ Year10, 
               data = marginalp, 
               prior = c(# for intercept 
                 prior(normal(0, 1), class = "Intercept"), 
                 # for slope
                 prior(normal(0, 1), class = "b"), 
                 # for sigma
                 prior(student_t(4, 0, 1), class = "sigma")), 
               seed = 1340)
print(m1_norm)

# Binary Logistic Regression ----------------------------------------------
# coefficients in logistic regression can be relatively large
# t-distributions with a small degrees of freedom as a good balance 
# between heavy tails and efficiency for MCMC sampling

m1_bern <- brm(marginal_p ~ Year10, 
               data = marginalp, 
               family = bernoulli(link = "logit"), 
               prior = prior(student_t(4, 0, .875), class = "b"), 
               # Note: no sigma 
               seed = 1340)

draws_beta0 <- as.matrix(m1_bern, pars = "b_Intercept")
logistic_beta0 <- plogis(draws_beta0)

# Summarize the posterior distribution
psych::describe(logistic_beta0)

mcmc_areas(m1_bern, pars = "b_Intercept", 
           transformations = list("b_Intercept" = "plogis"), bw = "SJ")

# Directly from the posterior of logistic(beta0):
quantile(logistic_beta0, probs = c(.05, .95))

# Transform the credible interval limits of beta0:
plogis(posterior_interval(m1_bern, pars = "Intercept"))

# InterpretingAs Odds Ratio
draws_beta1 <- as.matrix(m1_bern, pars = "b_Year10")
exp_beta1 <- exp(draws_beta1)
# Summarize the posterior distribution
psych::describe(exp_beta1)

# Directly from the posterior of exp(beta):
quantile(exp_beta1, probs = c(.05, .95))

# Or you can exponentiate the credible interval of beta:
exp(posterior_interval(m1_bern, pars = "Year10"))

# Model Checking
pp_check(m1_bern, type = "error_binned", nsamples = 9)

# Classification Error
m1_pred <- predict(m1_bern, type = "response")[ , "Estimate"]
m1_pred <- as.numeric(m1_pred > mean(marginalp$marginal_p))
# Classification table
(classtab_m1 <- table(predicted = m1_pred, observed = marginalp$marginal_p))

# Average classification accuracy
(acc_m1 <- sum(diag(classtab_m1)) / sum(classtab_m1))

# Using the pROC package
pROC::roc(response = marginalp$marginal_p, 
          predictor = predict(m1_bern, type = "response")[ , "Estimate"], 
          plot = TRUE, print.auc = TRUE)


# Binomial Logistic Regression --------------------------------------------
marginalp_agg <- 
  summarise(group_by(marginalp, Year10), 
            marginal_p = sum(marginal_p), n = n())
head(marginalp_agg)

# using a binomial distribution for the outcome, instead of Bernoulli:
m1_bin <- brm(marginal_p | trials(n) ~ Year10, 
              data = marginalp_agg, 
              family = binomial(link = "logit"), 
              prior = prior(student_t(4, 0, .875), class = "b"), 
              # Note: no sigma 
              seed = 1340)

pp_check(m1_bin, type = "intervals")
# the fit wasn’t particularly good in this case
