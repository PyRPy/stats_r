
# 7.2-10-------------------------------------------------------------------

dat <- read.table("E7_2-10.txt", quote="\"", comment.char="")
race <- dat$V1

# mean of race time difference
mu = mean(race) # 0.07875

# lower bond for mu
mu + qt(0.05, length(race)-1) * sd(race) / sqrt(length(race)) # -0.01043263

# no effects, since lower bound is less than zero.
