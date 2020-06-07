# Chapter 06 Comparisons of several mulivariate means 

# Example 6.1 Checking for a mean difference with paired observati --------
# waste water effluent data BOD and SS
effl=read.table("Data/T6-1.dat")
head(effl)
n=nrow(effl)

# obtain data for two differences
dBOD=effl[,1]-effl[,3]
dSS=effl[,2]-effl[,4]
deff=cbind(dBOD,dSS)

dmean=colMeans(deff)
dmean
Sd=cov(deff)
Sd

p=ncol(deff)

# find the t square 
tsq= n*t(dmean)%*%solve(Sd)%*%dmean

tsq
xc=(p*(n-1)/(n-p))*qf(.95,p,n-p) 
xc
# since tsq is larger, we reject null hypothesis
(tsq > xc) # true, reject Ho


# confidence intervals
BODL=dmean[1]-sqrt(xc*Sd[1,1]/n)
BODU=dmean[1]+sqrt(xc*Sd[1,1]/n)
c(BODL,BODU)

SSL=dmean[2]-sqrt(xc*Sd[2,2]/n)
SSU=dmean[2]+sqrt(xc*Sd[2,2]/n)
c(SSL,SSU) 


# Example 6.2 Testing for equal treatments in a repeated measures  --------
hbt=read.table("Data/T6-2.dat")
head(hbt)
n=nrow(hbt)

# The contrast matrix
mC=matrix(c(-1,1,1,-1,-1,-1,1,1,-1,1,-1,1),3,4)
mC
xmean=colMeans(hbt)
xcov=cov(hbt)
xcov
cxmean=mC%*%colMeans(hbt)
cxcov=mC%*%cov(hbt)%*%t(mC)
cxcov
q=ncol(hbt)

tsq= n*t(cxmean)%*%solve(cxcov)%*%cxmean
tsq

xcon=((q-1)*(n-1)/(n-q+1))*qf(.95,q-1,n-q+1) 
xcon
(tsq > xcon)
# since tsq is larger, we reject null hypothesis

# confidence intervals
c1L=cxmean[1]-sqrt(xcon*cxcov[1,1]/n)
c1U=cxmean[1]+sqrt(xcon*cxcov[1,1]/n)
c(c1L,c1U)

c2L=cxmean[2]-sqrt(xcon*cxcov[2,2]/n)
c2U=cxmean[2]+sqrt(xcon*cxcov[2,2]/n)
c(c2L,c2U)

c3L=cxmean[3]-sqrt(xcon*cxcov[3,3]/n)
c3U=cxmean[3]+sqrt(xcon*cxcov[3,3]/n)
c(c3L,c3U)
# find ones that do include 0
# how about a linear regression analysis on this four treatment ?


# 6.3 Compare mean vectors from two populations ---------------------------


# Example 6.3 construct a confidence region for diff of two means ---------
# soap x1 lather, x2 = mildness;
# need library to plot ellipse
library(ellipse)
p = 2
n1=50
n2=50
xbar1=c(8.3,4.1)
mS1=matrix(c(2,1,1,6),2,2)
xbar2=c(10.2,3.9)
mS2=matrix(c(2,1,1,4),2,2)

# pooled variances
mSp=((n1-1)*mS1+(n2-1)*mS2)/(n1+n2-2)
mSp

c2=(p*(n1+n2-2)/(n1+n2-p-1))*qf(.95,p,n1+n2-p-1)
c2

# eigen values / vectors
soap_eig=eigen(mSp)
soap_eig

hlflen1= sqrt(soap_eig$value[1]*(1/n1+1/n2)*c2)
hlflen1

hlflen2= sqrt(soap_eig$value[2]*(1/n1+1/n2)*c2)
# the second eigen vector from R is the negative of that in text
hlflen2

csq=(1/n1+1/n2)*c2  #constant for ellipse
dbar=xbar1-xbar2
eli=ellipse(mSp,centre=xbar1-xbar2,t=sqrt(csq),npoint=500)
plot(eli,cex=.3,xlab="lather",ylab="mildness",
     xlim=c(-3,0),ylim=c(-1,1.5),
     type="l",lty=1.5,bty="n")
points(-1.9,.2,pch=19,cex=1.1)
segments(-2.233,-.901,-1.57,1.301) 
# from xbar1-xbar2-hlflen*e1 to xbar1-xbar2+hlflen*e1
segments(-2.523,.389,-1.277,.011)

#calculation of axes to graph  ellipse
xbar1-xbar2

xbar1-xbar2-hlflen1*soap_eig$vector[,1]
xbar1-xbar2+hlflen1*soap_eig$vector[,1]
xbar1-xbar2-hlflen2*soap_eig$vector[,2]
xbar1-xbar2+hlflen2*soap_eig$vector[,2] 


# Example 6.4 Calculate simultaneous CI for the diff in mean comp. --------
# need library to plot ellipse
# electricty usage : on-peak/off-peak, with AC / without AC
library(ellipse)
n1=45
n2=55
p=2
xbar1=c(204.4,556.6)
mS1=matrix(c(13825.3,23823.4,23823.4,73107.4),2,2)
xbar2=c(130.0,355.0)
mS2=matrix(c(8632.0,19616.7,19616.7,55964.5),2,2)
mSp=((n1-1)*mS1+(n2-1)*mS2)/(n1+n2-2)
mSp
c2=(p*(n1+n2-2)/(n1+n2-p-1))*qf(.95,p,n1+n2-p-1)
c2
elect_eig=eigen(mSp)
elect_eig
hlflen1= sqrt(elect_eig$value[1]*(1/n1+1/n2)*c2)
hlflen1
hlflen2= sqrt(elect_eig$value[2]*(1/n1+1/n2)*c2)
# the second eigen vector from R is the negative of that in text
hlflen2

csq=(1/n1+1/n2)*c2  #constant for ellipse
eli=ellipse(mSp,centre=xbar1-xbar2,t=sqrt(csq),npoint=500)
plot(eli,cex=.3,xlim=c(0,150),ylim=c(0,350),
     xlab="difference of means for on-peak consumption",
     ylab="difference of means for off-peak consumption",
     type="l",lty=1.5,bty="n")

# confidence intervals
xdif1L=xbar1[1]-xbar2[1] -sqrt(c2)*sqrt((1/n1+1/n2)*mSp[1,1])
xdif1U=xbar1[1]-xbar2[1] +sqrt(c2)*sqrt((1/n1+1/n2)*mSp[1,1])
c(xdif1L,xdif1U)
xdif2L=xbar1[2]-xbar2[2] -sqrt(c2)*sqrt((1/n1+1/n2)*mSp[2,2])
xdif2U=xbar1[2]-xbar2[2] +sqrt(c2)*sqrt((1/n1+1/n2)*mSp[2,2])
c(xdif2L,xdif2U)


# Example 6.5 Large sample proc for inference on the diff in means --------
n1=45
n2=55
p=2
xbar1=c(204.4,556.6)
mS1=matrix(c(13825.3,23823.4,23823.4,73107.4),2,2)
xbar2=c(130.0,355.0)
mS2=matrix(c(8632.0,19616.7,19616.7,55964.5),2,2)
mSn1n2=mS1/n1+mS2/n2
mSn1n2

# critital value
c2=qchisq(.95,p)
c2

elect_eig=eigen(mSn1n2)
elect_eig
hlflen1= sqrt(elect_eig$value[1]*(1/n1+1/n2)*c2)
hlflen1
hlflen2= sqrt((1/n1+1/n2)*elect_eig$value[2]*c2)
# the second eigen vector from R is the negative of that in text
hlflen2

# confidence intervals
xdif1L=xbar1[1]-xbar2[1] -sqrt(c2)*sqrt(mSn1n2[1,1])
xdif1U=xbar1[1]-xbar2[1] +sqrt(c2)*sqrt(mSn1n2[1,1])
c(xdif1L,xdif1U)
xdif2L=xbar1[2]-xbar2[2] -sqrt(c2)*sqrt(mSn1n2[2,2])
xdif2U=xbar1[2]-xbar2[2] +sqrt(c2)*sqrt(mSn1n2[2,2])
c(xdif2L,xdif2U)

#T square
Tsq=t(xbar1-xbar2)%*%solve(mSn1n2)%*%(xbar1-xbar2)

c(Tsq,c2)

# most critical linear combination
xcoef=solve(mSn1n2)%*%(xbar1-xbar2) 
xcoef
