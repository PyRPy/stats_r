# Chapter 7 Multivariate linear regression models -------------------------

# Example 7.3 Calculate the least squares estimates, resid, rss -----------

# data as single column
y=c(1,4,3,8,9)
Z=matrix(c(1,1,1,1,1,0,1,2,3,4),5,2)
Z
t(Z)%*%Z
solve(t(Z)%*%Z)
t(Z)%*%y
bhat=solve(t(Z)%*%Z)%*%t(Z)%*%y # use matrix multiplication
bhat
yhat=Z%*%bhat
yhat
epsi=round(y-yhat,2) # residuals
epsi
t(epsi)%*%epsi # residual sum of squares

# using lm command
Dat=data.frame(cbind(Z,y))
colnames(Dat)=c("constant","z","y")
model=lm(y~z,data=Dat)
summary(model)


# Example 7.4 Fitting a regression model to real-estate data --------------

Dat=read.table("Data/T7-1.dat")
head(Dat)
names(Dat)=c("size","assessed","price")

# use the lm command
fit=lm(price~ size+assessed,data=Dat) # removed anova()
summary(fit)
# From the  summary we obtain estimated standard error = 2.8518
# From the  file fit, extract betahat3

beta2L=coef(fit)[3]-qt(.975,17)*.28518
beta2U=coef(fit)[3]+qt(.975,17)*.28518
c(beta2L,beta2U)
# alternatively
confint(fit) 


# Example 7.5 Test importance of additional predictors --------------------
Dat=read.table("Data/T7-2.dat",colClasses=c("factor","factor","numeric"))
# colClasses=c("factor","factor","numeric")
names(Dat)=c("location","gender","service")


# for the full model with interactions.
# See output table for sums of squares error
anova(lm(service~ location*gender,data=Dat)) 

# dropping the interaction term, we get the
#sum of squares error for the reduced model
anova(lm(service~ location+gender,data=Dat))

# F statistic is 
F= (3419.1-2977.4)/2/(2977.4/12)
Pval=1-pf(F,2,12)
c(F,Pval)

#to plot the residuals, use
model=lm(service~location*gender,data=Dat)
plot(fitted(model),residuals(model),ylab="Residual",xlab="Fitted",pch=19,col="blue",bty="n")
