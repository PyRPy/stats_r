
# E9_2_1.R ----------------------------------------------------------------

observed_table = matrix(c(95,36,71,21,45,32,
                          53,26,43,18,32,28), 
                        nrow = 2, ncol = 6, byrow = T)

rownames(observed_table) <- c('Group1', 'Group2')
colnames(observed_table) <- c('Cat1', 'Cat2', 'Cat3', 'Cat4', 'Cat5', 'Cat6')

observed_table
#        Cat1 Cat2 Cat3 Cat4 Cat5 Cat6
# Group1   95   36   71   21   45   32
# Group2   53   26   43   18   32   28

X <- chisq.test(observed_table)
X

X$expected
#        Cat1 Cat2 Cat3 Cat4 Cat5 Cat6
# Group1 88.8 37.2 68.4 23.4 46.2   36
# Group2 59.2 24.8 45.6 15.6 30.8   24

# X-squared = 3.2305, df = 5, p-value = 0.6645