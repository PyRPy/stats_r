
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
