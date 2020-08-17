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

# to plot the residuals, use
model=lm(service~location*gender,data=Dat)
plot(fitted(model),residuals(model),
     ylab="Residual",xlab="Fitted",
     pch=19,col="blue",bty="n")


# Example 7.6 Intervals for mean response and prediction ------------------

Dat=read.table("Data/T7-3.dat")
names(Dat)=c("orders","addel","CPU")

model=lm(CPU~orders+addel,data=Dat)
summary(model)

# to find confidence interval and predict
newdata = data.frame(orders=130,addel=7.5)
predict(model,newdata,interval="confidence")
predict(model,newdata,interval="prediction")


# Example 7.7 Residual plots ----------------------------------------------

Dat=read.table("Data/T7-3.dat")
names(Dat)=c("orders","addel","CPU")

model=lm(CPU~orders+addel,data=Dat)

plot(fitted(model),residuals(model),
     xlab=expression(hat(y)),
     ylab="residual",
     bty="n",pch=19,col="blue") 

with(Dat,
     plot(orders,residuals(model),
          bty="n",pch=19,col="blue",xlab=expression(z[2]) )
)
with(Dat,
     plot(addel,residuals(model),
          bty="n",pch=19,col="blue",xlab=expression(z[2]))
) 


# Example 7.8 Fit a multivariate straight-line regression model -----------

# data as single column
Y=matrix(c(1,4,3,8,9,-1,-1,2,3,2),5,2)
Z=matrix(c(1,1,1,1,1,0,1,2,3,4),5,2)
t(Z)%*%Z
solve(t(Z)%*%Z)
t(Z)%*%y
Bhat=solve(t(Z)%*%Z)%*%t(Z)%*%Y
Bhat
Yhat=Z%*%Bhat
Yhat
Epsilh=round(Y-Yhat,2)
Epsilh
t(Epsilh)%*%Yhat
t(Epsilh)%*%Epsilh
t(Y)%*%Y
t(Yhat)%*%Yhat

# using lm command
Dat=data.frame(cbind(Z,Y))
colnames(Dat)=c("constant","z1","y1","y2")
model=lm(cbind(y1,y2)~z1,data=Dat)
summary(model)


# Example 7.10 Confidence ellipse and pred ellipse for bivariate ----------

library(ellipse)
Dat=read.table("Data/T7-3.dat")
# From Table 7.3 in Example 7-6
Z=matrix(c(1,1,1,1,1,1,1,123.5,146.1,133.9,128.5,151.5,136.2,92,
           + 2.108,9.213,1.905,.815,1.061,8.603,1.125),7,3)
n=nrow(Z)
y2=c(301.8,396.1,328.2,307.4,362.4,369.5,229.1)
Dat2=data.frame(cbind(Dat,y2))
names(Dat2)=c("orders","addel","y1","y2")
Y=as.matrix(Dat2[,3:4])
n=nrow(Z)
m=ncol(Y)
r=ncol(Z)-1
Bet=solve(t(Z)%*%Z)%*%t(Z)%*%Y
S= t(Y-Z%*%Bet)%*%(Y-Z%*%Bet)/(n-r-1)
z0=c(1,130,7.5)

# center of ellipse is
cent=t(Bet)%*%z0
# For the confidence ellipsoid is
c2con=(m*(n-r-1)/(n-r-m))*qf(.95,m,n-r-m)*t(z0)%*%solve(t(Z)%*%Z)%*%z0
eli=ellipse(S,centre=cent,t=sqrt(c2con),npoint=5000)
plot(eli,cex=.3,lty=1.5,bty="n",xlim=c(140,160),ylim=c(330,370),type="l")
# For the prediction ellipsoid
c2pred=(m*(n-r-1)/(n-r-m))*qf(.95,m,n-r-m)*(1+t(z0)%*%solve(t(Z)%*%Z)%*%z0)
elip=ellipse(S,centre=cent,t=sqrt(c2pred),npoint=5000)
# to plot on same graph
lines(elip)


# Example 7.11 Best linear predictor, mse, multiple corr coefs ------------

# enter mean and covariance matrix. Then partition
mu=c(5,2,0)
Sig=matrix(c(10,1,-1,1,7,3,-1,3,2),3,3)
mu1=mu[1]
mu2=mu[2:3]
sig11=Sig[1,1]
sig21=Sig[2:3,1]
sig22=Sig[2:3,2:3]
sig12=sig21
solve(sig22)%*%sig21
beta0=mu[1]-t(solve(sig22)%*%sig21)%*%mu[2:3]
beta0
mse=sig11-sig12%*%solve(sig22)%*%sig12
mse

# The multiple correlation coefficient is
rhoYZ =sqrt(sig12%*%solve(sig22)%*%sig12/sig11)
rhoYZ

# as a check
Dhalf=diag(sqrt(diag(Sig)))
Rhoinv=Dhalf%*%solve(Sig)%*%Dhalf
sqrt(1-1/Rhoinv[1,1]) 


# Example 7.12 Max likelihood estimate of regression func -----------------
# single response
# we calculate from original data to keep more decimals
Dat=read.table("Data/T7-3.dat")
n=nrow(Dat)
muh=colMeans(Dat)
S=cov(Dat)

# label partition so response variable is first
muh1=muh[3]
muh2=muh[1:2]
S11=S[3,3]
S21=S[1:2,3]
S22=S[1:2,1:2]
beth=solve(S22)%*%S21
bet0h=muh1-t(beth)%*%muh2
c(bet0h,beth)

# the mle of mse is
mlemse=((n-1)/n)*(S11-t(S21)%*%solve(S22)%*%S21)
mlemse
