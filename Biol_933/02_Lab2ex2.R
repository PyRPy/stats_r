#BIOL933
#Lab 2
#Example 2

# This script illustrates the use of R to calculate densities (critical values) and
# their corresponding probabilities for common distributions 

#Normal distribution
Nvalue<-qnorm(0.975)
Nvalue
pNvalue<-pnorm(1.959964)
pNvalue

#t distribution
Tvalue<-qt(0.975,10)
Tvalue
pTvalue<-pt(2.228139,10)
pTvalue

#chi^2 distribution
Chivalue<-qchisq(0.975,10)
Chivalue
pChivalue<-pchisq(20.48318,10)
pChivalue

#F distribution
Fvalue<-qf(0.975,10,4)
Fvalue
pFvalue<-pf(8.843881,10,4)
pFvalue

#visualizing distributions
par(mfrow=c(2,2))
plot(function(x) dnorm(x), -6, 6, ylim = c(0, 0.5), main = "Z - Density")
plot(function(x) dt(x, df = 10), -6, 6, ylim = c(0, 0.5), main = "t - Density")
plot(function(x) dchisq(x, df = 10), 0, 50, ylim = c(0, 0.1), main = "chi^2 - Density")
plot(function(x) df(x, df1 = 10, df2 = 4), 0, 15, ylim = c(0, 0.7), main = "F - Density")

#generating random numbers from a normal distribution
rNvalues<-rnorm(10,5,2)
rNvalues
