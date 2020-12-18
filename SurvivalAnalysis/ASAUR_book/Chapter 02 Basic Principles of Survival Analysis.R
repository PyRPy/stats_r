
# 2.1 The Hazard and Survival Functions -----------------------------------

library(survival)

tm <- c(0,
	1/365, # first day of life 
	7/365, # seventh day of life 
	28/365, # fourth week of life 
	1:110) # subsequent years

hazMale <- survexp.us[,"male","2004"]
hazFemale <- survexp.us[,"female","2004"] 

# 2.2 Other Representations of a Survival Distribution --------------------

# 2.3 Mean and Median Survival Time ---------------------------------------

# 2.4 Parametric Survival Distributions -----------------------------------
weibSurv <- function(t, shape, scale) {
  pweibull(t, shape=shape, scale=scale, lower.tail=F)
}

curve(weibSurv(x, shape=1.5, scale=1/0.03), 
      from = 0, to = 80, ylim=c(0,1), 
      ylab="Survival probability", xlab="Time") # single quote

weibHaz <- function(x, shape, scale) {
  dweibull(x, shape=shape, scale=scale)/pweibull(x, shape=shape, scale=scale, lower.tail=F)
}

curve(weibHaz(x, shape=1.5, scale=1/0.03), from=0, to=80,
      ylab="Hazard", xlab="Time", col="red")

tt.weib <- rweibull(1000, shape=1.5, scale=1/0.03) 
mean(tt.weib)
median(tt.weib)	

# theory
gamma(1 + 1/1.5)/0.03 # mean
(log(2)^(1/1.5))/0.03 # median


gammaHaz <- function(x, shape, scale) {
  dgamma(x, shape=shape, scale=scale)/pgamma(x, shape=shape, scale=scale, lower.tail=F)
}
curve(gammaHaz(x, shape=1.5, scale=1/0.03), from=0, to=80, 
      ylab="Hazard", xlab="Time", col="red")


# 2.5 Computing the Survival Function from the Hazard Function ------------


