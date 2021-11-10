
# E9_2_5.R ----------------------------------------------------------------

observed_table = matrix(c(262,231,10,
                          302,205, 5), 
                        nrow = 2, ncol = 3, byrow = T)

rownames(observed_table) <- c('Male', 'Female')
colnames(observed_table) <- c('Favor', 'Oppose', 'No Opinion')

observed_table

X <- chisq.test(observed_table)
X

X$expected

# X-squared = 5.9747, df = 2, p-value = 0.05042

qchisq(1-0.05, 2) # 5.991465
