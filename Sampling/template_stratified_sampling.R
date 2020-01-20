# Stratified random sampling from data frame
# ref : https://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame
set.seed(506)
n <- 10000
dt <- data.table(age = sample(1:5, n, T),
                 lc = rbinom(n, 1, 0.5),
                 ants = rbinom(n, 1, 0.7))
with(dt, table(age, lc))

head(dt)


# use sample_n from dplyr  ------------------------------------------------

library(dplyr)
set.seed(506)
out2 <- dt %>%
  group_by(age, lc) %>% 
  sample_n(30)
head(out2)
with(out2, table(age, lc))


# split-apply-combine strategy --------------------------------------------

sp <- split(dt, list(dt$age, dt$lc))
head(sp)
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 30, FALSE), ])
out <- do.call(rbind, samples)
head(out)
with(out, table(age, lc))
