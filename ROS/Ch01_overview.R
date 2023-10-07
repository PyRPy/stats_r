
# 1.2 Why learn regression? -----------------------------------------------
# elections and the economy
library(rstanarm)
library(arm)
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

# Electric Company
electric_wide <- read.table("ROS_Data/electric_wide.txt", header = TRUE)
head(electrid_wide)

# linear model
attach(electric_wide)
post_test <- c(treated_posttest, control_posttest)
pre_test <- c(treated_pretest, control_pretest)
grade <- rep(electric_wide$grade, 2)
treatment <- rep(c(1,0), rep(length(treated_posttest),2))
supp <- rep(NA, length(treatment))
n_pairs <- nrow(electric_wide)
pair_id <- rep(1:n_pairs, 2)
supp[treatment==1] <- ifelse(supplement=="Supplement", 1, 0)
n <- length(post_test)
electric <- data.frame(post_test, pre_test, grade, treatment, supp, pair_id)
head(electric)
#write.csv(electric, root("ElectricCompany/data","electric.csv"))
#electric <- read.csv(root("ElectricCompany/data","electric.csv"))

fit_3 <- stan_glm(post_test ~ treatment + pre_test + treatment:pre_test,
                  subset=(grade==4), data=electric, refresh = 0)
print(fit_3)


# Peacekeeping example ----------------------------------------------------
library(foreign)
peace <- read.dta("ROS_Data/pk&pkept_old.dta")

colnames(peace)

# What percentage of countries had returned to civil wars:
censored <- peace$morewar==0
badness <- log(peace$hazard1)
peacekeepers <- peace$pk_dum==1
cfdate <- peace$cfdate
faildate <- peace$faildate
faildate[is.na(faildate)&!is.na(cfdate)] <- "2004-12-31"
delay <- (as.numeric(faildate) - as.numeric(cfdate))/365.24
ok <- peace$pcw==1 & !is.na(delay)
##ok <- cease_fire & !is.na(badness) & peace$t0 > 1979-12-31 & peace$pcw==1

# EDA
print(sum(ok))
print(mean(censored[ok]))
print(unique(peace[ok,3]))  # country names
print(mean(censored[ok & !peacekeepers]))
table(peacekeepers[ok], censored[ok])

par(mfrow=c(1,2))
time <- as.numeric (cfdate)
hist(time[peacekeepers&ok])
hist(time[!peacekeepers&ok])
par(mfrow=c(1,1))


# Simple Causal -----------------------------------------------------------
# Simulated data from linear model
SEED <- 1151
set.seed(SEED)
N <- 50
x <- runif(N, 1, 5)
y <- rnorm(N, 10 + 3*x, 3)
x_binary <- ifelse(x<3, 0, 1)
data <- data.frame(N, x, y, x_binary)

# Regression with binary predictor
lm_1a <- lm(y ~ x_binary, data = data)
display(lm_1a)

# Regression with continuous predictor
lm_1b <- lm(y ~ x, data = data)
display(lm_1b)

# Simulated data from nonlinear model
set.seed(SEED)
y <- rnorm(N, 5 + 30*exp(-x), 2)
data$y <- y

# Classical regression with continuous predictor
lm_2a <- lm(y ~ x, data = data)
display(lm_2a)

# Simulated data from two groups
set.seed(SEED)
N <- 100
z <- rep(0:1, N/2)
xx <- ifelse(z==0, rnorm(N, 0, 1.2)^2, rnorm(N, 0, .8)^2)
yy <- rnorm(N, 20 + 5*xx + 10*z, 3)
data <- data.frame(xx, z, yy)
lm_2 <- lm(yy ~ xx + z, data=data)
display(lm_2)


# Helicopters -------------------------------------------------------------
# plot it first, data is correct?
helicopters <- read.table("ROS_Data/helicopters.txt", header=TRUE)
head(helicopters)
M1 <- lm(time_sec ~ width_cm + length_cm, data = helicopters)
display(M1)
summary(M1)
plot(helicopters$width_cm, helicopters$time_sec)
