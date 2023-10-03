
# 1.2 Why learn regression? -----------------------------------------------
# elections and the economy
library(rstanarm)
hibbs <- read.table("ROS_Data/hibbs.dat", header = T)
head(hibbs)
#   year growth  vote inc_party_candidate other_candidate
# 1 1952   2.40 44.60           Stevenson      Eisenhower
# 2 1956   2.89 57.76          Eisenhower       Stevenson
# 3 1960   0.85 49.91               Nixon         Kennedy
# 4 1964   4.21 61.34             Johnson       Goldwater
# 5 1968   3.02 49.60            Humphrey           Nixon

with(data = hibbs,
     plot(growth, vote, xlab="growth in personal income",
          ylab="party vote share"))

M1 <- stan_glm(vote ~ growth, data = hibbs)
abline(coef(M1), col="green")
print(M1)
