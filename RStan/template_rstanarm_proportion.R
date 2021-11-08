
# Proportions using Beta Regression with rstanarm -------------------------
# https://cran.r-project.org/web/packages/rstanarm/vignettes/betareg.html


library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())

# Gasoline Data
library(rstanarm)
data("GasolineYield", package = "betareg")
head(GasolineYield)
hist(GasolineYield$yield)

gas_fit1 <- stan_betareg(yield ~ temp + batch, 
                         data = GasolineYield, 
                         link = "logit",
                         seed = 12345)
gas_fit2 <- stan_betareg(yield ~ temp + batch | pressure,
                         data = GasolineYield, link = "logit",
                         seed = 12345)
round(coef(gas_fit1), 2)
round(coef(gas_fit2), 2)

# check post sampling distributions
bayesplot_grid(
  pp_check(gas_fit1), pp_check(gas_fit2), 
  xlim = c(0,1),  
  ylim = c(0,5), 
  titles = c("gas_fit1", "gas_fit2"),
  grid_args = list(ncol = 2)
)

# expected log predictive distribution 
gas_loo1 <- loo(gas_fit1)
gas_loo2 <- loo(gas_fit2)
loo_compare(gas_loo1, gas_loo2)
