# https://uc-r.github.io/t_test
library(ggplot2) # for data midwest
# One-sample t-test -------------------------------------------------------
head(midwest)
t.test(midwest$percollege, mu=32, alternative = "less")


# Two-sample t-test -------------------------------------------------------
dat = subset(midwest, state == "MI" | state == "OH")
with(data = dat, boxplot(percollege ~ state))
t.test(percollege ~ state, data = dat)

# 10.3 - Paired T-Test ----------------------------------------------------

head(sleep)
boxplot(extra ~ group, data = sleep)
t.test(extra ~ group, data = sleep, paired = TRUE)