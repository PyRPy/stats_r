# Chapter 1 Multivariate Data and Multivariate Analysis
library(MVA)

# 1.3 Types of variables and the possible problem of ----------------------
head(hypo)
hypo[1:2, c("health", "weight")]

# 1.3.1 Missing values


# 1.4 Some multivariate data sets -----------------------------------------
library(HSAUR2)
head(measure)
# write.csv(measure, "measure.csv")

head(pottery)
# write.csv(pottery, "pottery.csv")

head(exam)
# write.csv(exam, "exam.csv")

head(USairpollution)
write.csv(USairpollution, "USairpollution.csv")

# 1.5 Covariances, correlations, and distances ----------------------------
# 1.5.1 Covariances
cov(measure[,c("chest", "waist", "hips")])
cov(subset(measure, gender == "female")[, c("chest", "waist", "hips")])
cov(subset(measure, gender == "male")[, c("chest", "waist", "hips")])

# 1.5.2 Correlations
cor(measure[, c("chest", "waist", "hips")])

# 1.5.3 Distances
dist(scale(measure[, c("chest", "waist", "hips")], center = FALSE))[1:12]


# 1.6 The multivariate normal density function ----------------------------
x <- measure[, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, MARGIN = 1, function(x) t(x-cm)%*%solve(S)%*%(x-cm))

# Normal probability plots of chest, waist, and hip measurements
par(mfrow=c(1, 3))
qqnorm(measure[,"chest"], main = "chest") 
qqline(measure[,"chest"])
qqnorm(measure[,"waist"], main = "waist")
qqline(measure[,"waist"])
qqnorm(measure[,"hips"], main = "hips")
qqline(measure[,"hips"])
dev.off()

# Chi-square plot of generalised distances for body measurements data
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d),
      xlab = expression(paste(chi[3]^2, " Quantile")),
      ylab = "Ordered distances")
abline(a = 0, b = 1)

# Normal probability plots for USairpollution data.
par(mfrow = c(4, 2))
sapply(colnames(USairpollution), function(x) {
   qqnorm(USairpollution[[x]], main = x)
   qqline(USairpollution[[x]])
})

# ??2 plot of generalised distances for USairpollution data.
x <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 6),
      sd <- sort(d),
      xlab = expression(paste(chi[6]^2, " Quantile")),
      ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))

oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)


# 1.7 Summary -------------------------------------------------------------

# analyze data simutaneously
# normal or not for each one
# probability density function
