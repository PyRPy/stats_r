# chapter 04 Multivariate Normal Distribution 

# Example 4.8 Linear combinations of random vectors -----------------------

xmu=c(3,-1,1)
Sigma=matrix(c(3,-1,1,-1,1,0,1,0,2),3,3)
xc=c(.5,.5,.5,.5)
xb=c(1,1,1,-3)

meanc=sum(xc)*xmu
varc=sum(xc**2)*Sigma

meanb=sum(xb)*xmu
varb=sum(xb**2)*Sigma

covbc=sum(xb*xc)*Sigma # all zeros


# Example 4.9 constructing a Q-Q plot -------------------------------------

x=c(-1,-.1,.16,.41,.62,.8,1.26,1.54,1.71,2.3)
n=length(x)
xprob=c((seq(n)-.5)/n)
xns=qnorm(xprob)
plot(xns,x,bty="n",xlab="normal score",ylab="observation",
     pch=19,col="blue")


# Example 4.10 A Q-Q plot for radiation data ------------------------------

rad=as.matrix(read.table("Data/T4-1.dat"),nrows=42)
n=length(rad)
xprob=c((seq(n)-.5)/n)
ns=qnorm(xprob)


# ties result in a string of horizontal points.
# here there are many ties and it takes several lines of code
rads=sort(rad)
nst=ns
nst[1]=(ns[1]+ns[2])/2
nst[3]=(ns[3]+ns[4]+ns[5])/3
nst[7]=(ns[7]+ns[8]+ns[9]+ns[10]+ns[11])/5
nst[13]=(ns[13]+ns[14]+ns[15])/3
nst[16]=(ns[16]+ns[17])/2
nst[18]=(ns[18]+ns[19]+ns[20]+ns[21]+ns[22]+ns[23]+ns[24]+ns[25]+ns[26])/9
nst[29]=(ns[29]+ns[30]+ns[31])/3
nst[32]=(ns[32]+ns[33])/2
nst[34]=(ns[34]+ns[35]+ns[36])/3
nst[37]=(ns[37]+ns[38]+ns[39]+ns[40])/4
nst[41]=(ns[41]+ns[42])/2
plot(nst[c(6,12,27,28)],rads[c(6,12,27,28)],
     xlim=c(-2.4,2.4),pty="n",pch=19,ylim=c(0,.45),
     xlab="normal scores", ylab="radiation with door closed")
text(nst[c(1,16,32,41)],rads[c(1,16,32,41)],expression(2))
text(nst[c(3,13,29,34)],rads[c(3,13,29,34)],expression(3))
text(nst[37],rads[37],expression(4))
text(nst[7],rads[7],expression(5))
text(nst[18],rads[18],expression(9)) 


# Example 4.11 a correlation coefficient test for normality ---------------

x=c(-1,-.1,.16,.41,.62,.8,1.26,1.54,1.71,2.3)
n=length(x)
xprob=c((seq(n)-.5)/n)
xns=qnorm(xprob)
xr=cor(xns,x) # check with table 4.2 


# Example 4.12 checking bivariate normality -------------------------------

dcomp=read.table("Data/P1-4.dat")
names(dcomp)=c("sales","profits","assests")
n=nrow(dcomp)
colMeans(dcomp[,1:2]) # use first two variables
cov(dcomp[,1:2])

# subtract col means from 3 financial measures
dcompc<-apply(dcomp[,1:2],2,scale,scale=F,center=T)
dsq=diag(dcompc%*%solve(cov(dcomp[,1:2]))%*%t(dcompc))
dsq
tblevalue=qchisq(.5,2)


# Example 4.13 constructing a chi-square plot -----------------------------

dcomp=read.table("Data/P1-4.dat")
names(dcomp)=c("sales","profits","assests")
n=nrow(dcomp)
colMeans(dcomp[,1:2]) #use first two variables
cov(dcomp)


# subtract col means from 3 financial measures
dcompc<-apply(dcomp[,1:2],2,scale,scale=F,center=T)
dsq=diag(dcompc%*%solve(cov(dcomp[,1:2]))%*%t(dcompc))
dsq

# sort squared differences
dsqs=sort(dsq)
qc=qchisq((c(1:n)-.5)/n,2)
plot(qc,dsqs,bty="n",
     xlab="chi square score",
     ylab="squared distance",
     pch=19,col="blue",ylim=c(0,5))


# Example 4.14 Evaluating multivariate normality for a four var -----------

stiff=read.table("Data/T4-3.dat")
names(stiff)=c("x1","x2","x3","x4","dsq")
n=length(stiff[,1])

# we will check the last column although you could just plot dsqs versus 
# chisqs subtract col means from 4 stiffness measures
stiffc<-apply(stiff[,1:4],2,scale,scale=F,center=T)
dsq=diag(stiffc%*%solve(cov(stiffc))%*%t(stiffc))

# sort values of dsq
dsqs=sort(dsq)
chisqs=qchisq((seq(1:n)-.5)/n, 4) # 4 variables
plot(chisqs,dsqs,bty="n",xlim=c(0,14),
     xlab=expression(q[4]((j-.5)/30)),ylab=expression(d^2),ylim=c(0,17))
points(chisqs,dsqs,pch=19,col="blue")


# Example 4.15 Detecting outliers in the data on lumber -------------------

stiff=read.table("Data/T4-3.dat")
names(stiff)=c("x1","x2","x3","x4","dsq")

# we ignore the column dsq and standardize the 4 stiffness measures
stiffz<-apply(stiff[,1:4],2,scale,scale=T,center=T)
round(stiffz,1)

# the dsq based on stiffz is the same as for the original data but
# we subtract col means from 4 stiffness measures
stiffc<-apply(stiff[,1:4],2,scale,scale=F,center=T)
dsq=diag(stiffc%*%solve(cov(stiffc))%*%t(stiffc))
round(dsq,2)
plot(dsq, xlab = "obs", ylab = "distance")

pairs(stiff[,1:4])


# Example 4.16 Determine a power transformation for univariate ------------

rad=as.matrix(read.table("Data/T4-1.dat"),nrows=42)

# define function
mllik<-function (parm,x) {
  n=length(x)
  xnew=(x**parm-1)/parm
  mlogl=-(n/2)*log(((n-1)/n)*var(xnew))+(parm-1)*sum(log(x))
}

lamb=c(-(10:1),1:15)/10
valik=sapply(lamb,mllik,x=rad)

valik0=(-n/2)*log(((n-1)/n)*var(log(rad)))-1*sum(log(rad)) #value for lambda=0

cbind(lamb,round(valik,2))

c(0,round(valik0,2))

# to plot we use more points and restrict to 0 to .5
lambplot=c(1:500)/1000
valikplot=sapply(lambplot,mllik,x=rad)
plot(lambplot,valikplot,type="l",
     xlab=expression(lambda),
     ylab="log-likelihood",bty="n")
segments(0.28,104.5,0.28,mllik(.28,rad),lty=2)

# We now construct the normal scores plot of the data transformed 
# with lambda=.25
n=length(rad)
xprob=c((seq(n)-.5)/n)
ns=qnorm(xprob)


# ties result in a string of horizontal points.
# here there are many ties and it takes several lines of code
rads=sort(4*((rad)^{.25}-1))
nst=ns
nst[1]=(ns[1]+ns[2])/2
nst[3]=(ns[3]+ns[4]+ns[5])/3
nst[7]=(ns[7]+ns[8]+ns[9]+ns[10]+ns[11])/5
nst[13]=(ns[13]+ns[14]+ns[15])/3
nst[16]=(ns[16]+ns[17])/2
nst[18]=(ns[18]+ns[19]+ns[20]+ns[21]+ns[22]+ns[23]+ns[24]+ns[25]+ns[26])/9
nst[29]=(ns[29]+ns[30]+ns[31])/3
nst[32]=(ns[32]+ns[33])/2
nst[34]=(ns[34]+ns[35]+ns[36])/3
nst[37]=(ns[37]+ns[38]+ns[39]+ns[40])/4
nst[41]=(ns[41]+ns[42])/2
plot(nst[c(6,12,27,28)],rads[c(6,12,27,28)],
     xlim=c(-2.4,2.4),bty="n",pch=19,ylim=c(-3,-.5),
     xlab="normal scores", ylab="radiation with door closed")
text(nst[c(1,16,32,41)],rads[c(1,16,32,41)],expression(2))
text(nst[c(3,13,29,34)],rads[c(3,13,29,34)],expression(3))
text(nst[37],rads[37],expression(4))
text(nst[7],rads[7],expression(5))
text(nst[18],rads[18],expression(9))


# Example 4.17 determine power transformations for bivariate data ---------

radC=read.table("Data/T4-1.dat")
radO=read.table("Data/T4-5.dat")
X=cbind(radC,radO)

#function defined
mllik<-function(parm1,parm2,x) {
  n=length(x[,1])
  x1new=(x[,1]**parm1-1)/parm1
  x2new=(x[,2]**parm2-1)/parm2
  xnew=cbind(x1new,x2new)
  
  mlogl=-(n/2)*log(det(((n-1)/n)*cov(xnew)))+(parm1-1)*sum(log(x[,1]))+
    (parm2-1)*sum(log(x[,2]))
}
lamb1=c(1:125/250)
lamb2=c(1:125/250)
valLik=matrix(nrow=125,ncol=125)
for(i in 1:125)
{
  for (j in 1:125)
  {
    valLik[i,j]=mllik(lamb1[i],lamb2[j], X)
  }
}
contour(lamb1,lamb2,valLik,levels=c(222,223,224,225,225.5,226,226.5,226.91)
        ,xlab=expression(lambda[1]),ylab=expression(lambda[2]))


n=length(X[,2])
xprob=c((1:n-.5)/n)
ns=qnorm(xprob)

# ties result in a string of horizontal points.here there are many ties
rads=sort(X[,2])
nst=ns
nst[1]=(ns[1]+ns[2])/2
nst[4]=(ns[4]+ns[5]+ns[6])/3
nst[8]=(ns[8]+ns[9]+ns[10]+ns[11]+ns[12])/5
nst[7]=(ns[7]+ns[8]+ns[9]+ns[10]+ns[11])/5
nst[13]=(ns[13]+ns[14]+ns[15]+ns[16]+ns[17]+ns[18]+ns[19]+ns[20]+ns[21])/9
nst[22]=(ns[22]+ns[23]+ns[24]+ns[25]+ns[26]+ns[27])/6
nst[28]=(ns[28]+ns[29])/2
nst[30]=(ns[30]+ns[31])/2
nst[34]=(ns[34]+ns[35]+ns[36]+ns[37])/4
plot(nst[c(3,7,32,33,38,39,40,41,42)],rads[c(3,7,32,33,38,39,40,41,42)],
     xlim=c(-2.4,2.4),bty="n",pch=19,
     ylim=c(0,.65),
     xlab="normal scores",ylab="radiation")
text(nst[c(1,28,30)],rads[c(1,28,30)],expression(2))
text(nst[4],rads[4],expression(3))
text(nst[34],rads[34],expression(4))
text(nst[8],rads[8],expression(5))
text(nst[13],rads[18],expression(13))

# the box-cox transformation preserves order when lamda is positive 
# so normal scores are unchanged

# ties result in a string of horizontal points.here there are many ties
xnew=(X[,2]**.3-1)/.3
rads=sort(xnew)
nst=ns
nst[1]=(ns[1]+ns[2])/2
nst[4]=(ns[4]+ns[5]+ns[6])/3
nst[8]=(ns[8]+ns[9]+ns[10]+ns[11]+ns[12])/5
nst[7]=(ns[7]+ns[8]+ns[9]+ns[10]+ns[11])/5
nst[13]=(ns[13]+ns[14]+ns[15]+ns[16]+ns[17]+ns[18]+ns[19]+ns[20]+ns[21])/9
nst[22]=(ns[22]+ns[23]+ns[24]+ns[25]+ns[26]+ns[27])/6
nst[28]=(ns[28]+ns[29])/2
nst[30]=(ns[30]+ns[31])/2
nst[34]=(ns[34]+ns[35]+ns[36]+ns[37])/4
plot(nst[c(3,7,32,33,38,39,40,41,42)],rads[c(3,7,32,33,38,39,40,41,42)],
     xlim=c(-2.5,2.5),bty="n",
     pch=19,ylim=c(-2.5,0),
     xlab="normal scores",
     ylab="transformed door open radiation")
text(nst[c(1,28,30)],rads[c(1,28,30)],expression(2))
text(nst[4],rads[4],expression(3))
text(nst[34],rads[34],expression(4))
text(nst[8],rads[8],expression(5))
text(nst[13],rads[13],expression(9))

