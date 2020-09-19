# 6.1 Descriptive Statistics ----------------------------------------------

# Example 6.1-5 Times between calls to 911 --------------------------------
# histograms and pdf of X
calls <- scan("Data-R/Ex6_1-5.txt")
hist(calls, freq = FALSE, ylim = c(0, 0.05))
x <- seq(0, 104, by=0.1)
fx <- 1/20*exp(-x/20)
lines(x, fx, col="blue")


# plot(cumsum(table(calls)/length(calls)))
# plot(table(calls))
# counts <- table(calls)
# counts
# str(counts)

# theoratical and emperical cdfs
Fn = ecdf(calls) 
plot(Fn) 
Fx <- 1- exp(-x/20)
lines(x, Fx, col="blue")


# ref: http://www.r-tutor.com/elementary-statistics/quantitative-data/cumulative-relative-frequency-graph