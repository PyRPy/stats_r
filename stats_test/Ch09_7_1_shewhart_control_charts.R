

# quality control charts --------------------------------------------------
library(qcc)
dat <- read.delim("Ex9_7-1.txt", header=FALSE)
colnames(dat) <- c("Group", "Measurement")
head(dat)

# reshape data or organize data
dat2 <- with(dat, qcc.groups(Measurement, Group))
head(dat2)

# x-bar chart
q1 = qcc(dat2[1:10, ], type = "xbar")
plot(q1, chart.all = FALSE)

# R chart
q2 <- qcc(dat2[1:10, ], type = 'R')
summary(q2)

# S chart
q3 <- qcc(dat2[1:10, ], type = 'S')
