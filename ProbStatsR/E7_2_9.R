
# 7.2-9 -------------------------------------------------------------------

male_pre = c(11.1, 19.50, 14.00, 8.3, 12.4, 
             7.89, 12.1, 8.3, 12.31, 10.00)
male_post = c(9.97, 15.80, 13.02, 9.28, 11.51, 
              7.4, 10.70, 10.40, 11.40, 11.95)

female_pre = c(22.90, 31.60, 27.70, 21.70, 19.36, 
               25.03, 26.9, 25.75, 23.63, 25.06)
female_post = c(22.89, 33.47, 25.75, 19.80, 18.00, 
                22.33, 25.26, 24.90, 21.80, 24.28)

# 90% CI for male difference
male_diff = male_post - male_pre
sdd_male = sd(male_diff)
mu_male_diff = mean(male_diff)
mu_male_diff + c(qt(0.05, length(male_diff)-1), 
                 qt(1-0.05, length(male_diff)-1)) * sdd_male / sqrt(length(male_diff))
# -1.4495936  0.5555936

# 90% CI for female difference
female_diff = female_post - female_pre
sdd_female = sd(female_diff)
mu_female_diff = mean(female_diff)
mu_female_diff + c(qt(0.05, length(female_diff)-1), 
                 qt(1-0.05, length(female_diff)-1)) * sdd_female / sqrt(length(female_diff))
# -1.8630221 -0.3669779

# based on the data, male difference CI include zero, so no difference with 90% confidence
# female CI does not include zero, it shows decreasing effect.

# qq plot

qqnorm(male_diff)
qqline(male_diff)

qqnorm(female_diff)
qqline(female_diff)

# not normal distribution.
