
# 17.1 - Test For Homogeneity ---------------------------------------------
# reference: 
# https://www.datacamp.com/community/tutorials/contingency-analysis-r
observed_table = matrix(c(300, 240, 300, 360,
                          200, 160, 200, 240), 
                        nrow = 2, ncol = 4, byrow = T)

rownames(observed_table) <- c('Male', 'Female')
colnames(observed_table) <- c('Business', 'Engineer', 'LibArts', 'Science')

observed_table
#        Business Engineer LibArts Science
# Male        300      240     300     360
# Female      200      160     200     240

X <- chisq.test(observed_table)
X

X$expected


# Different data ----------------------------------------------------------

observed_table = matrix(c(240, 480, 120, 360,
                          240, 80, 320, 160), 
                        nrow = 2, ncol = 4, byrow = T)

rownames(observed_table) <- c('Male', 'Female')
colnames(observed_table) <- c('Business', 'Engineer', 'LibArts', 'Science')

observed_table
#        Business Engineer LibArts Science
# Male        240      480     120     360
# Female      240       80     320     160

X <- chisq.test(observed_table)
X

#        Pearson's Chi-squared test
# 
# data:  observed_table
# X-squared = 389.11, df = 3, p-value < 2.2e-16
X$expected
