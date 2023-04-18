
# Wilcoxon ranking test ---------------------------------------------------
# for non-normal distribution sample data

# example 8-4-1
# Ho: m = 6.2
# Ha: m < 6.2
time_interval <- read.table("Ex8_4-1.txt", quote="\"", comment.char="")
names(time_interval) <- "length"

wilcox.test(time_interval$length, mu = 6.2, alternative = "less")
# p-value = 0.8477

# example 8-4-2 sunfish length
Ho: m = 3.7
Ha: m > 3.7

sunfish <- read.table("Ex8_4-2.txt", quote="\"", comment.char="")
names(sunfish) <- "length"

wilcox.test(sunfish$length, mu = 3.7, alternative = "greater")
boxplot(sunfish$length)
# p-value = 0.1162

# example 8-4-4 random numbers
Ho: m = 160
Ha: m > 160

nums <- read.table("Ex8_4-4.txt", quote="\"", comment.char="")
names(nums) <- "value"
wilcox.test(nums$value, mu = 160, alternative = "greater")
# p-value = 0.06487

# example 8-4-5 body fat difference before and after the semester
Ho: m = 0
Ha: m > 0

fat <- read.table("Ex8_4-5.txt", quote="\"", comment.char="")
names(fat) <- "pct"
wilcox.test(fat$pct, mu = 0, alternative = "greater")
# p-value = 0.2505, same as pvalue in the textbook
boxplot(fat)

# example 8-4-6 cinnamon weights from two companies
Ho: ma = mb
Ha: ma < mb
cinnamon <- read.delim("Ex8_4-6.txt", header=FALSE)
names(cinnamon) <- c("A", "B")

wilcox.test(cinnamon$A, cinnamon$B, alternative = "less")
# p-value = 0.02494

boxplot(cinnamon)
