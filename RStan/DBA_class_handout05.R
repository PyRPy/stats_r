
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
