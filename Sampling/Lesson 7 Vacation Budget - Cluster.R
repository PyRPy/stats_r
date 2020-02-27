# Chapter 7 Vacation Budget -----------------------------------------------

Vacation <- read.delim("Vacation.txt") 
head(Vacation)
#please change the code here to your local address where you save the file
#hint: use "/" if "\" shows in the local address and does not work.

attach(Vacation)
Mi<-NumberofHouseholds
yi<-TotalBudgetperCluster

plot(Mi, yi, main="Scatter plot of Yi vs Mi",
             xlab="Cluster size",
             ylab="total for the cluster",
             pch=19)

# check linear relationship and whether pass through origin
r_est_check<-lm(yi~Mi)
summary(r_est_check) # not significant

# Calculate r, estimate mean and its variance
r=sum(yi)/sum(Mi)
mu_hat_r=r
M=3100
N=400
n=24
Var_muhatr=N*(N-n)/(n*(M^2))*(sum((yi-r*Mi)^2))/(n-1)
Var_muhatr

# estimate total and its variance
tau_hat_r=r*M
tau_hat_r
Var_tauhatr=N*(N-n)/(n*(n-1))*(sum((yi-r*Mi)^2))
Var_tauhatr

# unbiased estimator for mean and its variance for comparison purpose 
mu_hat=N*sum(yi)/(n*M)
mu_hat
Var_muhat=N*(N-n)/(n*(M^2))*sum((yi-mean(yi))^2)/(n-1)
Var_muhat
