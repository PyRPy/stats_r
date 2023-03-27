
# sample correlation coefficients -----------------------------------------

test_scores <- read.delim("Ex6_5-1.txt", header=FALSE)
colnames(test_scores) <- c("pre", "final")

# correlation test - assume x, y come from binormial distribution
res <- cor.test(test_scores$pre, test_scores$final, method = "pearson")
res

# scatter plot
plot(test_scores$pre, test_scores$final,
     main = "preliminary vs final scores")

# normality test on 'pre' and 'final' variables
shapiro.test(test_scores$pre) # p-value = 0.9324

shapiro.test(test_scores$final) # p-value = 0.6434

# kendall and spearman options for method are used when samples data
# are not normal distribution

# linear regression model
reg_model <- lm(final ~ pre, data = test_scores)
summary(reg_model)
