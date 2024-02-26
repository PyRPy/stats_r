
#  18. CAUSAL INFERENCE AND RANDOMIZED EXPERIMENTS ------------------------
# https://avehtari.github.io/ROS-Examples/Sesame/sesame.html
library("rstanarm")
library("brms")
# Set random seed for reproducability
SEED <- 1234

# load data
sesame <- read.csv("ROS_Data/sesame.csv")
head(sesame)

# Compliance
(sesame_tab <- table(sesame[,c('watched','encouraged')]))
round(prop.table(sesame_tab, margin=2), digits=2)

# Wald estimator
itt_zt <- stan_glm(watched ~ encouraged, data=sesame, seed=SEED, refresh=0)
print(itt_zt, digits=2)

itt_zy <- stan_glm(postlet ~ encouraged, data=sesame, refresh=0)
print(itt_zy, digits=1)

# Calculate Wald estimate, ie the ratio of the above two estimates
wald_est <- coef(itt_zy)["encouraged"] / coef(itt_zt)["encouraged"]
round(wald_est, digits=1)
