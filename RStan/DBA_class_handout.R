# Course Handouts for Bayesian Data Analysis Class ------------------------

# https://bookdown.org/marklhc/notes_bookdown/


# Chapter 1 Introduction --------------------------------------------------


# Chapter 2 Bayesian Inference --------------------------------------------


library(tidyverse)
data("Aids2", package = "MASS")
set.seed(15)
Aids2_sub <- Aids2 %>% sample_n(10)
Aids2_sub
