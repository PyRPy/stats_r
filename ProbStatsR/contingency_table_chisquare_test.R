# Chi-square test for two category variables ------------------------------

library(MASS)
head(survey)
str(survey)
table_smoke_exer <- table(survey$Smoke, survey$Exer)
table_smoke_exer


# Null Hypothesis ---------------------------------------------------------

# Ho: smoke and exercise are independent
# Ha: smoke and exercise are dependent

chisq.test(table_smoke_exer)

if (chisq.test(table_smoke_exer)$p.value > 0.05) {
  cat("There is no sufficient evidence to reject Ho, so Ho is true. \n")
} else {
  cat("There is sufficient evidence to reject Ho, so Ha is true. \n")
}

# http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence