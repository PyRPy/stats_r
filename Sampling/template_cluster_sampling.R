# template for Cluster sampling
# ref: https://stackoverflow.com/questions/15087988/cluster-sampling-with-r-or-sas
# create toy example data
x <- data.frame(
  groupname = sample(x = 1:2000, size = 155000,replace = TRUE),
  anothercol = 1,
  andanothercol = "hi"
)
head(x)

# proportion of each group
groupwise.prob <- table(x$groupname)/nrow(x)
prob.frame <- data.frame(groupwise.prob)
head(prob.frame)

names(prob.frame)[1] <- "groupname"
names(prob.frame)[2] <- "prob"
x <- merge(x, prob.frame, all.x = TRUE)
head(x)

recs.to.samp <- sample(1:nrow(x), size = 600, replace = FALSE,
                       prob = x$prob)

y <- x[recs.to.samp, ]
head(y)
