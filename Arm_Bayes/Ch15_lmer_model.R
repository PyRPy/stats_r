
# linear mixed effects model ----------------------------------------------
# https://psyteachr.github.io/ug3-stats/introducing-linear-mixed-effects-models.html

library(lme4)
data("sleepstudy")
head(sleepstudy)

library(tidyverse)

just_308 <- sleepstudy %>%
  filter(Subject == "308")

ggplot(just_308, aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9)

# remove day 0 and day 1
sleep2 <- sleepstudy %>%
  filter(Days >= 2L) %>%
  mutate(days_deprived = Days - 2L)


# complete pooling --------------------------------------------------------

cp_model <- lm(Reaction ~ days_deprived, sleep2)

summary(cp_model)

coef(cp_model)


# no pooling --------------------------------------------------------------

sleep2 %>% pull(Subject) %>% is.factor()
np_model <- lm(Reaction ~ days_deprived + Subject + days_deprived:Subject,
               data = sleep2)

summary(np_model)

# get the intercepts and slopes for each subject
all_intercepts <- c(coef(np_model)["(Intercept)"],
                    coef(np_model)[3:19] + coef(np_model)["(Intercept)"])

all_slopes  <- c(coef(np_model)["days_deprived"],
                 coef(np_model)[20:36] + coef(np_model)["days_deprived"])

ids <- sleep2 %>% pull(Subject) %>% levels() %>% factor()

# make a tibble with the data extracted above
np_coef <- tibble(Subject = ids,
                  intercept = all_intercepts,
                  slope = all_slopes)

np_coef

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_abline(data = np_coef,
              mapping = aes(intercept = intercept,
                            slope = slope),
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

np_coef %>% pull(slope) %>% t.test()


# mixed model -------------------------------------------------------------

pp_mod <- lmer(Reaction ~ days_deprived + (days_deprived | Subject), sleep2)

summary(pp_mod)


# Interpreting lmer() output and extracting estimates ---------------------
# Fixed effects
fixef(pp_mod)

sqrt(diag(vcov(pp_mod)))

# OR, equivalently using pipes:
# vcov(pp_mod) %>% diag() %>% sqrt()

# fixed effect estimate
tvals <- fixef(pp_mod) / sqrt(diag(vcov(pp_mod)))

tvals

# get the p-values
2 * (1 - pnorm(abs(tvals)))
confint(pp_mod)

# Random effects
sigma(pp_mod) # residual

# variance-covariance matrix for random factor Subject
VarCorr(pp_mod)[["Subject"]] # equivalently: VarCorr(pp_mod)[[1]]

diag(VarCorr(pp_mod)[["Subject"]]) # just the variances

ranef(pp_mod)[["Subject"]]

# make predictions --------------------------------------------------------
## create the table with new predictor values
newdata <- crossing(
  Subject = sleep2 %>% pull(Subject) %>% levels() %>% factor(),
  days_deprived = 0:7)

head(newdata, 17)
newdata2 <- newdata %>%
  mutate(Reaction = predict(pp_mod, newdata))

ndat <- crossing(Subject = sleep2 %>% pull(Subject) %>% levels() %>% factor(),
                 days_deprived = 8:10) %>%
  mutate(Reaction = predict(pp_mod, newdata = .))

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_line(data = bind_rows(newdata2, ndat),
            color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:10) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")
