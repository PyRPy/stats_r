
# E9_2_1.R ----------------------------------------------------------------

observed_table = matrix(c(95,36,71,21,45,32,
                          53,26,43,18,32,28,
                          130,75,136,33,61,65), 
                        nrow = 3, ncol = 6, byrow = T)

rownames(observed_table) <- c('Group1', 'Group2', 'Group3')
colnames(observed_table) <- c('Cat1', 'Cat2', 'Cat3', 'Cat4', 'Cat5', 'Cat6')

observed_table

X <- chisq.test(observed_table)
X

X$expected

# X-squared = 10.176, df = 10, p-value = 0.4252

qchisq(1-0.025, 2*5) # 20.48318
