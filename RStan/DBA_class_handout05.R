
# Chapter 5 Group Comparisons ---------------------------------------------
# https://bookdown.org/marklhc/notes_bookdown/group-comparisons.html
library(tidyverse)

## Data - lie response time
lies <- readxl::read_excel("ENDFILE.xlsx")

# Rescale response time from ms to sec
lies <- lies %>% 
  mutate_at(vars(LDMRT:TEMRT), ~ . / 1000)

# Describe the data
psych::describe(lies %>% select(Age, LDMRT:TEMRT))

# Plot the data
psych::pairs.panels(lies %>% 
                      select(Age, Gender, LDMRT:TEMRT))

## Between-Subject Comparisons

lies %>% 
  select(PP, Gender, LDMRT, TDMRT) %>% 
  gather(key = "veracity", value = "RT", LDMRT:TDMRT) %>% 
  ggplot(aes(x = RT, col = veracity)) + 
  geom_density(bw = "SJ") + 
  facet_wrap(~ Gender)

## Independent sample t-test

t.test(lies$LDMRT[which(lies$Gender == "man")], 
       lies$LDMRT[which(lies$Gender == "vrouw")])

## Bayesian Normal Model
# graphs for the three priors:

p1 <- ggplot(tibble(mu = c(0, 5)), aes(x = mu)) + 
  stat_function(fun = function(x) dnorm(x, mean = 0.5, sd = 2.5) / 
                  (1 - pnorm(0, 0.5, sd = 2.5))) + 
  labs(x = expression(mu), y = "Density")
p2 <- ggplot(tibble(beta = c(-5, 5)), aes(x = beta)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 2.5)) + 
  labs(x = expression(beta), y = "Density")
p3 <- ggplot(tibble(sig = c(0, 5)), aes(x = sig)) + 
  stat_function(fun = function(x) dt(x, df = 2) * 2) + 
  labs(x = expression(sigma), y = "Density")

gridExtra::grid.arrange(p1, p2, p3, nrow = 2)

# STAN code for the model
library(rstan)
rstan_options(auto_write = TRUE)
# Exclude missing values
lies_cc <- drop_na(lies, LDMRT)

m1 <- stan("group_comparisons.stan", 
          data = list(N1 = sum(lies_cc$Gender == "man"), 
                      N2 = sum(lies_cc$Gender == "vrouw"), 
                      y1 = lies_cc$LDMRT[which(lies_cc$Gender == "man")], 
                      y2 = lies_cc$LDMRT[which(lies_cc$Gender == "vrouw")]))

# Use the `broom` package to generate nicely formatted table
print(m1, pars = c("mu_1", "mu_2", "beta", "sigma"))

#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# mu_1   1.81       0 0.10  1.60  1.74  1.80  1.87  2.00  1687    1
# mu_2   1.30       0 0.07  1.16  1.25  1.30  1.35  1.44  4433    1
# beta  -0.51       0 0.12 -0.75 -0.59 -0.51 -0.42 -0.26  1754    1
# sigma  0.46       0 0.04  0.39  0.43  0.46  0.49  0.55  2237    1

## Within-Subject Comparisons
lies %>% 
  select(PP, Gender, LDMRT, TDMRT) %>% 
  gather(key = "veracity", value = "RT", LDMRT:TDMRT) %>% 
  ggplot(aes(x = RT, col = veracity)) + 
  geom_density(bw = "SJ")

# paired t-test
t.test(lies$TDMRT, lies$LDMRT, paired = TRUE)

# Bayesian Normal Model
library(rstan)
group_comparison_paired = "
data {
  int<lower=0> N;  // number of observations
  vector[N] y1;  // response time (repeated measure 1);
  vector[N] y2;  // response time (repeated measure 2);
}
parameters {
  real<lower=0> mu_1;  // mean of group 1
  real beta;  // difference in means
  real<lower=0> sigma;  // residual SD
  vector[N] zu;  // individual difference parameters (scaled)
  real<lower=0> tau;  // standard deviation of indivdiual difference
}
transformed parameters {
  real<lower=0> mu_2 = mu_1 + beta; 
  vector[N] u = zu * tau;
}
model {
  y1 ~ normal(mu_1 + u, sigma);
  y2 ~ normal(mu_2 + u, sigma);
  // prior
  mu_1 ~ normal(0.5, 2.5);
  beta ~ normal(0, 2.5);
  sigma ~ student_t(4, 0, 2.5);
  zu ~ std_normal();
  // hyperprior
  tau ~ student_t(4, 0, 2.5);
}
generated quantities {
  real y1rep[N];
  real y2rep[N];
  real cohen_d = (mu_2 - mu_1) / sqrt(sigma^2 + tau^2);
  for (i in 1:N) {
    y1rep[i] = normal_rng(mu_1 + u[i], sigma);
    y2rep[i] = normal_rng(mu_2 + u[i], sigma);
  }
}
"
m4 <- stan(model_code = group_comparison_paired, 
           data = list(N = 63, 
                       y1 = lies_cc$TDMRT, 
                       y2 = lies_cc$LDMRT), 
           pars = "zu", include = FALSE, seed = 104134)

# Use the `broom` package to generate nicely formatted table
print(m4, pars = c("mu_1", "mu_2", "beta", "sigma", "tau", "cohen_d"))

plot(m4, pars = c("mu_1", "mu_2", "beta", "sigma", "tau", "cohen_d"))

# Posterior Predictive Check
library(bayesplot)
# Observed data
y1 <- lies_cc$TDMRT
y2 <- lies_cc$LDMRT
# Replicated data (randomly sampling 100)
y1rep <- as.matrix(m4, pars = "y1rep")[sample.int(2000, 100), ]
y2rep <- as.matrix(m4, pars = "y2rep")[sample.int(2000, 100), ]

ppc_dens_overlay(y1, yrep = y1rep)
ppc_dens_overlay(y2, yrep = y2rep)

ppc_intervals(y1, yrep = y1rep)
ppc_intervals(y2, yrep = y2rep)


# Using brms --------------------------------------------------------------
library(brms)
lies_long <- lies %>% 
  select(PP, Gender, LDMRT, TDMRT) %>% 
  gather(key = "veracity", value = "RT", LDMRT:TDMRT)

m2_brm <- brm(RT ~ veracity + (1 | PP), data = lies_long, 
              family = student(), 
              prior = c(prior(normal(0, 1), class = "b"), 
                        prior(student_t(4, 0, 2.5), class = "sd"), 
                        prior(student_t(4, 0, 2.5), class = "sigma")))


# sjPlot::tab_model(m2_brm, show.ci50 = FALSE)  # broken in newest version of `brms`
plot(m2_brm)
pp_check(m2_brm, nsamples = 100)

pp_check(m2_brm, type = "intervals_grouped", group = "veracity")
