
# 12.1 - One Variance -----------------------------------------------------

# Ho : sigma2 = 1600
# Ha : sigma2 > 1600

test_stat = (40 - 1) * 48.5^2 / 40^2
qchisq(1-0.05, 40-1)

# reject or not
ifelse(test_stat > qchisq(1-0.05, 40-1), "reject Ho", "not reject Ho")

# two sided test
qchisq(c(0.05, 1-0.05), 40-1)
# 25.69539 54.57223


# 12.2 - Two Variances ----------------------------------------------------

# Ho : sigma2_x = sigma2_y 
# Ha : sigma2_x =/= sigma2_y 

test_stat = 12.2^2 / 20.1^2
# 0.3684067

qf(c(0.025, 1-0.025), 29-1, 34-1)
# 0.4786899 2.0440729