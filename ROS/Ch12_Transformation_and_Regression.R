
# TRANSFORMATIONS AND REGRESSION ------------------------------------------
# https://avehtari.github.io/ROS-Examples/Mesquite/mesquite.html

library("foreign")
library("rstanarm")
library("loo")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

# set the seed for reproducing the results
SEED <- 4587

# load the data
mesquite <- read.table("ROS_Data/mesquite.dat", header=TRUE)
head(mesquite)

# use all predictors
fit_1 <- stan_glm(weight ~ diam1 + diam2 + canopy_height + total_height +
                    density + group,
                  data=mesquite,
                  seed=SEED,
                  refresh=0)
print(fit_1)

# loo
(loo_1 <- loo(fit_1)) # not stable ?

# K-fold-CV
(kfold_1 <- kfold(fit_1, K=10))

# fit the model on the logarithmic scale
fit_2 <- stan_glm(formula = log(weight) ~ log(diam1) + log(diam2) + log(canopy_height) +
                    log(total_height) + log(density) + group, data=mesquite)
(loo_2 <- loo(fit_2))

# Jacobian to adjust the predictive comparison
loo_2_with_jacobian <- loo_2
loo_2_with_jacobian$pointwise[,1] <- loo_2_with_jacobian$pointwise[,1] -
  log(mesquite$weight)

sum(loo_2_with_jacobian$pointwise[,1])
loo_compare(kfold_1, loo_2_with_jacobian)

# simulate replicated datasets
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
subset <- sample(n_sims, 100)
ppc_dens_overlay(mesquite$weight, yrep_1[subset,])
yrep_2 <- posterior_predict(fit_2)
ppc_dens_overlay(log(mesquite$weight), yrep_2[subset,])
# log scale is better


