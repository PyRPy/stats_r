
# Chapter 7 Evaluate model and cross validation ---------------------------
# https://users.aalto.fi/~ave/modelselection/betablockers.html
# https://github.com/avehtari/modelselection/blob/master/binom_odds_comparison.R

library(tidyr)
library(rstanarm)
library(loo)
library(ggplot2)
theme_set(bayesplot::theme_default())
library(ggridges)
library(bridgesampling)

# A group of patients were randomly assigned to treatment and control groups
# out of 674 patients receiving the control, 39 died
# out of 680 receiving the treatment, 22 died

# Data construction (grp2 is a dummy for considering intercepts)
d_bin2 <- data.frame(N = c(674, 680), y = c(39,22), grp2 = c(0,1))

# compute odds-ratio
fit_bin2 <- stan_glm(y/N ~ grp2, family = binomial(), data = d_bin2,
                     weights = N, refresh=0)

# extract parameters
samples_bin2 <- rstan::extract(fit_bin2$stanfit)
theta1 <- plogis(samples_bin2$alpha)
theta2 <- plogis(samples_bin2$alpha + samples_bin2$beta)
oddsratio <- (theta2/(1-theta2))/(theta1/(1-theta1))
ggplot() + geom_histogram(aes(oddsratio), bins = 50, fill = 'grey', color = 'darkgrey') +
  labs(y = '') + scale_y_continuous(breaks = NULL)

# find probability that odds-ratio is less than 1
print(mean(oddsratio < 1), 2) # 0.99


# Roaches cross-validation demo -------------------------------------------
# https://users.aalto.fi/~ave/modelselection/roaches.html
library(rstanarm)
library(brms)
library(cmdstanr)
options(mc.cores = 4)
library(loo)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))

# Poisson model -----------------------------------------------------------
# Load data
data("roaches")
head(roaches)
summary(roaches)

# Roach1 is very skewed and we take a square root
roaches$sqrt_roach1 <- sqrt(roaches$roach1)

# Fit with stan_glm
stan_glmp <- stan_glm(y ~ sqrt_roach1 + treatment + senior, offset = log(exposure2),
                      data = roaches, family = poisson,
                      prior = normal(0,2.5), prior_intercept = normal(0,5),
                      chains = 4, cores = 4, seed = 170400963, refresh=0)
# Plot posterior
mcmc_areas(as.matrix(stan_glmp), prob_outer = .999)

# Cross-validation checking
# leave-one-out cross-validation
loop <- loo(stan_glmp)
# Warning message:
# Found 16 observations with a pareto_k > 0.7.
# With this many problematic observations we recommend
# calling 'kfold' with argument 'K=10' to perform 10-fold cross-validation
# rather than LOO.

# plot(loop)
# 3 models by dropping each of the covariates out.
stan_glmm1p <- update(stan_glmp, formula = y ~ treatment + senior)
stan_glmm2p <- update(stan_glmp, formula = y ~ sqrt_roach1 + senior)
stan_glmm3p <- update(stan_glmp, formula = y ~ sqrt_roach1 + treatment)

loo_compare(loo(stan_glmm1p), loop)
loo_compare(loo(stan_glmm2p), loop)
loo_compare(loo(stan_glmm3p), loop)
# covrariates are relevant, but with hgih uncertainties

# Posterior predictive checking
# test the proportion of zeros predicted by the model and
# compare them to the observed number of zeros.
prop_zero <- function(y) mean(y == 0)
(prop_zero_test1 <- pp_check(stan_glmp, plotfun = "stat", stat = "prop_zero"))


# Negative binomial model -------------------------------------------------
# change the Poisson model to a more robust negative binomial model
stan_glmnb <- update(stan_glmp, family = neg_binomial_2)

# Analyse posterior
# parameters distribution
mcmc_areas(as.matrix(stan_glmnb), prob_outer = .999,
           pars = c("(Intercept)","sqrt_roach1","treatment","senior"))
# pair plot
mcmc_pairs(as.matrix(stan_glmnb),
           pars = c("(Intercept)","sqrt_roach1","treatment","senior"))

# Cross-validation checking
loonb <- loo(stan_glmnb)
loo_compare(loop, loonb)

mcmc_areas(as.matrix(stan_glmnb), prob_outer = .999,
           pars = c("reciprocal_dispersion"))

# Posterior predictive checking
# predict the proportion of zeros
prop_zero_test2 <- pp_check(stan_glmnb, plotfun = "stat", stat = "prop_zero")
prop_zero_test2

# Predictive relevance of covariates
stan_glmm1nb <- update(stan_glmm1p, family = neg_binomial_2)
stan_glmm2nb <- update(stan_glmm2p, family = neg_binomial_2)
stan_glmm3nb <- update(stan_glmm3p, family = neg_binomial_2)

loo_compare(loo(stan_glmm1nb), loonb)
loo_compare(loo(stan_glmm2nb), loonb)
loo_compare(loo(stan_glmm3nb), loonb)
# cross-validation is not good for detecting weak effects


# Poisson model with “random effects” -------------------------------------
# adding “random effects” for each individual
roaches$id <- 1:dim(roaches)[1]

# mixed model
stan_glmpr <- stan_glmer(y ~ sqrt_roach1 + treatment + senior + (1 | id),
                         offset = log(exposure2),
                         data = roaches, family = poisson,
                         prior = normal(0,2.5), prior_intercept = normal(0,5),
                         chains = 4, cores = 4, iter = 4000,
                         seed = 170400963, refresh=0)

# Analyse posterior
mcmc_areas(as.matrix(stan_glmpr), prob_outer = .999,
           pars = c("(Intercept)","sqrt_roach1","treatment","senior"))

# Cross-validation checking
(loopr <- loo(stan_glmpr))
plot(loopr)

# use K-fold-CV instead to re-fit the model 10 times,
# each time leaving out 10% of the observations
(kcvpr <- kfold(stan_glmpr, K=10)) # cannot converge or running > 5 mins

# comparing PSIS-LOO and K-fold-CV results
loo_compare(loonb, kcvpr)
# this difference could also be explained by K-fold-CV using only 90% of
# observations for the posteriors, while PSIS-LOO is using 99.6% of
# observations for the posteriors.

(kcvnb <- kfold(stan_glmnb, K=10))
loo_compare(kcvnb, kcvpr)
loo_compare(loonb, loopr)

loo_compare(waic(stan_glmnb), waic(stan_glmpr))

# Posterior predictive checking
(prop_zero_test3 <- pp_check(stan_glmpr, plotfun = "stat", stat = "prop_zero"))


# Poisson model with “random effects” and integrated LOO ------------------
poisson_re_int <- "poisson_re_integrate.stan"
writeLines(readLines(poisson_re_int))

# Compile the model, prepare the data, and sample.
# set_cmdstan_path(cmdstan_path())
# ---- make sure cmdstan is actually installed ---- #

modpri <- cmdstan_model(stan_file = poisson_re_int)
datap <- list(N=dim(roaches)[1], P=3,
              offsett=log(roaches$exposure2),
              X=roaches[,c('sqrt_roach1','treatment','senior')],
              y=roaches$y)
fitpri <- modpri$sample(data = datap, refresh=0, chains=8, parallel_chains=4)

mcmc_areas(as_draws_matrix(fitpri$draws(variables=c('alpha','beta','sigmaz'))), prob_outer = .999)

# Zero-inflated negative-binomial model
brm_glmznb <- brm(bf(y ~ sqrt_roach1 + treatment + senior + offset(log(exposure2)),
                     zi ~ sqrt_roach1 + treatment + senior + offset(log(exposure2))),
                  family=zero_inflated_negbinomial(), data=roaches,
                  prior=set_prior("normal(0,1)"), seed=170400963, refresh=500)

looznb <- loo(brm_glmznb)
loo_compare(loonb, looznb)

yrepnzb <- posterior_predict(brm_glmznb)
(prop_zero_test4 <- ppc_stat(y=roaches$y, yrepnzb, stat=function(y) mean(y==0)))

# Analyse posterior
mcmc_areas(as.matrix(brm_glmznb)[,1:8], prob_outer = .999)

# Predictive relevance of covariates
brm_glmm1znb <- brm(bf(y ~ treatment + senior + offset(log(exposure2)),
                       zi ~ treatment + senior + offset(log(exposure2))),
                    family=zero_inflated_negbinomial(), data=roaches,
                    prior=set_prior("normal(0,1)"), seed=170400963, refresh=500)

loo_compare(loo(brm_glmm1znb),looznb)
#              elpd_diff se_diff
# brm_glmznb     0.0       0.0
# brm_glmm1znb -57.8      10.1

# reference and future readings
# https://users.aalto.fi/~ave/modelselection/roaches.html
