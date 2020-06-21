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


# Example 6.6 The approx. Tsq distribution when sigma1 != sigma2 ----------
# still electricity problem
n1=45
n2=55
p=2
xbar1=c(204.4,556.6)
S1=matrix(c(13825.3,23823.4,23823.4,73107.4),2,2)
xbar2=c(130.0,355.0)
S2=matrix(c(8632.0,19616.7,19616.7,55964.5),2,2)
Sn1n2=S1/n1+S2/n2
Sn1n2

# the degrees of freedom calculation uses sum(diag(A) for trace(A).First
k1=n1**(-1)*(sum(diag((S1/n1)%*%solve(S1/n1+S2/n2)%*%(S1/n1)%*%solve(S1/n1+S2/n2)))+
                     ( sum(diag((S1/n1)%*%solve(S1/n1+S2/n2))))**2)
k2=n2**(-1)*(sum(diag((S2/n2)%*%solve(S1/n1+S2/n2)%*%(S2/n2)%*%solve(S1/n1+S2/n2)))+
                     ( sum(diag((S2/n2)%*%solve(S1/n1+S2/n2))))**2)
# estimate of degrees of freedom
nu=(p+p^2)/(k1+k2)
nu

# critical value
c2=(nu*p/(nu-p+1))*qf(.95,p,nu-p+1)

T2=t(xbar1-xbar2)%*%solve(Sn1n2)%*%(xbar1-xbar2)
c(T2,c2)


# 6.4 One way MANOVA ------------------------------------------------------

# Example 6.7 The SS decomposition for ANOVA ------------------------------

# data as single column
x=c(9,6,9,0,2,3,1,2)
tr=c(1,1,1,2,2,3,3,3)

# change data type to factor
ftr=factor(tr)
fit=aov(lm(x~ftr))
# the group means are
tapply(x,tr,mean)
mean(x)
round(residuals(fit), 1)

# show the matrix as in the text book
mat <- c(residuals(fit)[1:5], NA, residuals(fit)[6:8])
matrix(round(mat, 1), 3,3, byrow = T)
plot(x, residuals(fit))
abline(h = 0)


# Example 6.8 ANOVA table anf F-test for trt effects ----------------------

# As in Example 6.7
x=c(9,6,9,0,2,3,1,2)
tr=c(1,1,1,2,2,3,3,3)
# change data type to factor
ftr=factor(tr)
fit=aov(lm(x~ftr))
fit # how to get it by 'hands-on' calculations 

# From the table we calculate the observed F
F=(78/2)/(10/5)

g=3 # number of treatment
n=length(x) 
ctble=qf(.99,g-1,n-g)
c(F,ctble)
(F > ctble) # true; reject Ho: tau1 = tau2 = tau3 = 0 (not treatment effects) 


# Example 6.9 MANOVA table and Wilks' lambda - 3 means --------------------
# We first do the decompositions of the two variables separately
# For the first variable
x11=c(9,6,9)
x12=c(0,2)
x13=c(3,1,2)
x1=c(x11,x12,x13)
n1=length(x11)
n2=length(x12)
n3=length(x13)
xbar11=mean(x11)*c(rep(1,n1))
xbar12=mean(x12)*c(rep(1,n2))
xbar13=mean(x13)*c(rep(1,n3))
x1bar=mean(x1)
x1bar1_3=c(xbar11,xbar12,xbar13)
x1-x1bar1_3
SSobs1=t(x1)%*%x1
SSmean1=(n1+n2+n3)*x1bar**2
SStr1=t(x1bar1_3-x1bar)%*%(x1bar1_3-x1bar)
SSres1=t(x1-x1bar1_3)%*%(x1-x1bar1_3)
Totalcor1=SStr1+SSres1

# For the second variable
# data as single column
x=c(9,6,9,0,2,3,1,2)
tr=c(1,1,1,2,2,3,3,3)
# change data type to factor
ftr=factor(tr)
fit=aov(lm(x~ftr))


x21=c(3,2,7)
x22=c(4,0)
x23=c(8,9,7)
x2=c(x21,x22,x23)
xbar21=mean(x21)*c(rep(1,n1))
xbar22=mean(x22)*c(rep(1,n2))
xbar23=mean(x23)*c(rep(1,n3))
x2bar=mean(x2)
x2bar1_3=c(xbar21,xbar22,xbar23)
x2-x2bar1_3
SSobs2=t(x2)%*%x2
SSmean2=(n1+n2+n3)*x2bar**2
SStr2=t(x2bar1_3-x2bar)%*%(x2bar1_3-x2bar)
SSres2=t(x2-x2bar1_3)%*%(x2-x2bar1_3)
Totalcor2=SStr2+SSres2

# Calculate the cross product terms
SCPobs12=t(x2)%*%x1
SCPmean12=(n1+n2+n3)*x2bar*x1bar
SCPtr12=t(x2bar1_3-x2bar)%*%(x1bar1_3-x1bar)
SCPres12=t(x2-x2bar1_3)%*%(x1-x1bar1_3)

# Form matrices
W=matrix(c(SSres1,SCPres12,SCPres12,SSres2),2,2)
B=matrix(c(SStr1,SCPtr12,SCPtr12,SStr2),2,2)
Lamb=det(W)/det(B+W)
Lamb
g=3

# transformed test statistic
TLam=((1-sqrt(Lamb))/sqrt(Lamb))*(n1+n2+n3-g-1)/(g-1)
crit=qf(.99,2*(g-1),2*(n1+n2+n3-g-1))
c(TLam,crit)


# Example 6.10 A multivariate analysis of Wisc. nursing home data ---------
# the study is to investigate the effects of ownership or certification on costs
# X1 nursing labor cost, X2 dietary labor X3 plant O&M  X4 house keep and laundry
# group 1 private, 2 nonprofit, 3 government
# enter sample sizes, means and covariance matrices
n1=271
n2=138
n3=107
p=4
xbar1=c(2.066,.480,.082,.360)
xbar2=c(2.167,.596,.124,.418)
xbar3=c(2.273,.521,.125,.383)
S1=matrix(c(.291,-.001,.002,.010,-.001,.011,.000,.003,.002,.000,.001,.000,.010,.003,.000,.010),4,4)
S2=matrix(c(.561,.011,.001,.037,.011,.025,.004,.007,.001,.004,.005,.002,.037,.007,.002,.019),4,4)
S3=matrix(c(.261,.030,.003,.018,.030,.017,-.000,.006,.003,-.000,.004,.001,.018,.006,.001,.013),4,4)

# calculate statistics
W=(n1-1)*S1+(n2-1)*S2+(n3-1)*S3 # W different from text so different values for test statistic
W
xbar=(n1*xbar1+n2*xbar2+n3*xbar3)/(n1+n2+n3)
B=n1*(xbar1-xbar)%*%t(xbar1-xbar)+ n2*(xbar2-xbar)%*%t(xbar2-xbar)+n3*(xbar3-xbar)%*%t(xbar3-xbar)
Lamb=det(W)/det(B+W)
Lamb

# trasformed statistic to F
g=3
n=n1+n2+n3
T=((n-p-2)/p)*(1-sqrt(Lamb))/sqrt(Lamb)
crit=qchisq(.99,2*p)/(2*p)
c(T,crit)

# the approximation for log Lamb
Tlog=-(n-1-(p+g)/2)*log(Lamb)
critlog=qchisq(.99,p*(g-1))
c(Tlog,critlog) 

# page-309
# 6.11 Simultaneous interval for treatment differences - nursing h --------
# enter sample sizes, means and covariance matrices
n1=271
n2=138
n3=107
n = n1 + n2 + n3
p=4
g=3
xbar1=c(2.066,.480,.082,.360)
xbar2=c(2.167,.596,.124,.418)
xbar3=c(2.273,.521,.125,.383)
S1=matrix(c(.291,-.001,.002,.010,-.001,.011,.000,.003,.002,.000,.001,.000,.010,.003,.000,.010),4,4)
S2=matrix(c(.561,.011,.001,.037,.011,.025,.004,.007,.001,.004,.005,.002,.037,.007,.002,.019),4,4)
S3=matrix(c(.261,.030,.003,.018,.030,.017,-.000,.006,.003,-.000,.004,.001,.018,.006,.001,.013),4,4)

# calculate statistics
W=(n1-1)*S1+(n2-1)*S2+(n3-1)*S3 
# W different from text so confidence intervals different

xbar=(n1*xbar1+n2*xbar2+n3*xbar3)/(n1+n2+n3)
xbar
hatau1=xbar1-xbar
hatau2=xbar2-xbar
hatau3=xbar3-xbar
1-.05/(4*3*2) # 1 - a/2m

dif133=hatau1[3]-hatau3[3]
x133L=dif133-qt(.99792,n-g)*sqrt((1/n1+1/n3)*W[3,3]/(n-g))
x133U=dif133+qt(.99792,n-g)*sqrt((1/n1+1/n3)*W[3,3]/(n-g))
c(x133L,x133U)

x123L=hatau1[3]-hatau2[3]-qt(.99792,n-g)*sqrt((1/n1+1/n2)*W[3,3]/(n-g))
x123U=hatau1[3]+hatau2[3]+qt(.99792,n-g)*sqrt((1/n1+1/n2)*W[3,3]/(n-g))
c(x123L,x123U)  # no difference in means

x233L=hatau2[3]-hatau3[3]-qt(.99792,n-g)*sqrt((1/n2+1/n3)*W[3,3]/(n-g))
x233U=hatau2[3]-hatau3[3]+qt(.99792,n-g)*sqrt((1/n2+1/n3)*W[3,3]/(n-g))
c(x233L,x233U)


# Example 6.12 Testing equality of covariance matrices --------------------

# enter sample sizes, means and covariance matrices
n1=271
n2=138
n3=107
n=n1+n2+n3
p=4
g=3
xbar1=c(2.066,.480,.082,.360)
xbar2=c(2.167,.596,.124,.418)
xbar3=c(2.273,.521,.125,.383)
S1=matrix(c(.291,-.001,.002,.010,-.001,.011,.000,.003,.002,.000,.001,.000,.010,.003,.000,.010),4,4)
S2=matrix(c(.561,.011,.001,.037,.011,.025,.004,.007,.001,.004,.005,.002,.037,.007,.002,.019),4,4)
S3=matrix(c(.261,.030,.003,.018,.030,.017,-.000,.006,.003,-.000,.004,.001,.018,.006,.001,.013),4,4)
# calculate statistic for testing equality of covariance matrices
W=(n1-1)*S1+(n2-1)*S2+(n3-1)*S3
Sp=W/(n-g)
u=(1/(n1-1)+1/(n2-1)+1/(n3-1)-1/(n-g))*(2*p^2+3*p-1)/(6*(p+1)*(g-1))
M=(n-g)*log(det(Sp))-(n1-1)*log(det(S1))-(n2-1)*log(det(S2))-(n3-1)*log(det(S3))
CT=(1-u)*M
# df chi square
nu=p*(p+1)*(g-1)/2
crit=qchisq(.99,nu)
c(CT,crit)  # the P-value will be extremely small
# reject Ho. conclude that covariance matrices are not equivalent


# Ex 6.13 Two-way multivariate analysis of plastic film data --------------

Dat=read.table("Data/T6-4.dat")
names(Dat)=c("rate","additive","tear","gloss","opacity")
str(Dat)
head(Dat)

fit=manova(cbind(tear,gloss,opacity)~rate*additive,data=Dat)
summary(fit,test="Wilks")

# the anova's for each  response variable are
fit1=aov(tear~rate*additive,data=Dat)
summary(fit1)

fit2=aov(gloss~rate*additive,data=Dat)
summary(fit2)

fit3=aov(opacity~rate*additive,data=Dat)
summary(fit3)

# The two factors do not interact. The individual summary for tear
with(Dat,tapply(tear,list(rate),mean))
with(Dat,tapply(tear,list(rate),var))
with(Dat,tapply(tear,list(additive),mean))
with(Dat,tapply(tear,list(additive),var))

# for gloss
with(Dat,tapply(gloss,list(rate),mean))
with(Dat,tapply(gloss,list(rate),var))
