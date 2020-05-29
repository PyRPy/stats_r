# Chapter 05 Inference about a mean vector 

# Example 5.1 Evaluating T square -----------------------------------------

# enter data matrix
X=matrix(c(6,10,8,9,6,3),3,2)
xbar=colMeans(X)
mS=cov(X) 
xmu=c(9,5) # given, known

n=nrow(X)
tsq=n*t(xbar-xmu)%*%solve(mS)%*%(xbar-xmu)
p=ncol(X)
dfd=n-p
#the multiple of the F having p and n-p df is
c=(n-1)*p/(n-p)


# Example 5.2 Testing a multivariate mean vector with Tsq -----------------

sweat=read.table("Data/T5-1.dat")
names(sweat)=c("rate","sodium","potas")
head(sweat)

xbar=colMeans(sweat)
mS=cov(sweat)
xmu=c(4,50,10) # given as in Ho
n=nrow(sweat)
tsq=n*t(xbar-xmu)%*%solve(mS)%*%(xbar-xmu)
p=ncol(sweat)

# critical value is
c2=((n-1)*p/(n-p))*qf(.9,p,n-p) 
c(tsq,c2) # tsq > c2, reject Ho, confidence level = 0.9


# Example 5.3 Constructing a confidence ellipse for mu --------------------

#install packagae  ellipse to draw ellipse
library(ellipse)
radC=read.table("Data/T4-1.DAT",header=T)
radO=read.table("Data/T4-5.DAT",header=T)
radcl=radC**(.25)
radop=radO**(.25)
rad=cbind(radcl,radop)

n=nrow(rad)
p=ncol(rad)
c2=p*(n-1)*qf(.95,p,n-p)/(n-2)
xbar=colMeans(rad)
S=cov(rad)
solve(S)

# check if in ellipse
xmu=c(.562,.589)
tsq=n*t(xbar-xmu)%*%solve(S)%*%(xbar-xmu)

# draw ellipse
eig=eigen(S)
a1=xbar-sqrt(c2*eig$value[1]/n)%*% eig$vector[,1]
b1=xbar+sqrt(c2*eig$value[1]/n)%*% eig$vector[,1]
a2=xbar-sqrt(c2*eig$value[2]/n)%*% eig$vector[,2]
b2=xbar+sqrt(c2*eig$value[2]/n)%*% eig$vector[,2]

eli = ellipse(S, centre=xbar,t=sqrt(c2/n), npoint=5000)

plot(eli, cex=.3,bty="n", 
     xlab="Radiation door closed-fourth root", 
     ylab="Radiation door open-fourth root",
     xlim=c(.5,.7),ylim=c(.5,.7),
     type="l",lwd=2,col="blue")
points(xbar[1],xbar[2],pch=19)
segments(xbar[1],xbar[2],a1[1],a1[2])
segments(xbar[1],xbar[2],b1[1],b1[2])
segments(xbar[1],xbar[2],a2[1],a2[2])
segments(xbar[1],xbar[2],b2[1],b2[2])



# Example 5.4 Simultaneous CI as shadows of Conf. Ellipse -----------------

# requires library ellipse to draw ellipse
library(ellipse)
radC=read.table("Data/T4-1.dat")
radO=read.table("Data/T4-5.dat")
radcl=radC**(.25)
radop=radO**(.25)
rad=cbind(radcl,radop)

n=nrow(rad)
p=ncol(rad)
c2=p*(n-1)*qf(.95,p,n-p)/((n-p)*n)
xbar=colMeans(rad)
S=cov(rad)

# for door closed the endpoints of the shadow are
xbar[1]-sqrt(c2*S[1,1])
xbar[1]+sqrt(c2*S[1,1])

# for door open
xbar[2]-sqrt(c2*S[2,2])
xbar[2]+sqrt(c2*S[2,2])

eli = ellipse(S, centre=xbar,t=sqrt(c2), npoint=5000)

plot(eli, cex=.3,bty="n", 
     xlab="Radiation door closed-fourth root", 
     ylab="Radiation door open-fourth root",
     xlim=c(.5,.7),ylim=c(.5,.7),
     type="l",lwd=2,col="blue")

segments(.516,.45,.516,.56,lty=2)
segments(.612,.45,.612,.641,lty=2)
segments(.45,.651,.60,.651,lty=2)
segments(.45,.555,.522,.555,lty=2)


# Example 5.5 Constructing simultaneous CI and ellipse --------------------

library(ellipse)
coltest=read.table("Data/T5-2.DAT")
names(coltest) <- c("sSocialS", "verbal", "science")

n=nrow(coltest)
p=ncol(coltest)
c2=p*(n-1)*qf(.95,p,n-p)/(n-p)
xbar=colMeans(coltest)
S=cov(coltest)

# calculate endpoints of axes
eigv=eigen(S)
a1=xbar-sqrt(eigv$value[1]*c2)%*%eigv$vector[,1]
b1=xbar+sqrt(eigv$value[1]*c2)%*%eigv$vector[,1]
a2=xbar-sqrt(eigv$value[2]*c2)%*%eigv$vector[,2]
b2=xbar+sqrt(eigv$value[2]*c2)%*%eigv$vector[,2]
a3=xbar-sqrt(eigv$value[3]*c2)%*%eigv$vector[,3]
b3=xbar+sqrt(eigv$value[3]*c2)%*%eigv$vector[,3]

xl1=xbar[1]-sqrt(c2*S[1,1]/n)
xu1=xbar[1]+sqrt(c2*S[1,1]/n)

# for verbal
xl2=xbar[2]-sqrt(c2*S[2,2]/n)
xu2=xbar[2]+sqrt(c2*S[2,2]/n)

# for science
xl3=xbar[3]-sqrt(c2*S[3,3]/n)
xu3=xbar[3]+sqrt(c2*S[3,3]/n)
 
#draw ellipse x1 x2
xbar12=c(xbar[1],xbar[2])
S12=matrix(c(S[1,1],S[2,1],S[1,2],S[2,2]),2,2)

eli = ellipse(S12, centre=xbar12,t=sqrt(c2/n), npoint=5000)

plot(eli, cex=.3,bty="n", 
     xlim=c(500,550),ylim=c(50,60), 
     xlab="Social science", ylab="Verbal",
     type="l",lwd=2,col="blue")

segments(xl1,50-.5,xl1,xl2+1,lty=2)
segments(xu1,50-.5,xu1,xu2-1,lty=2)
segments(500-2,xl2,xl1+5,xl2,lty=2)
segments(500-2,xu2,xu1-5,xu2,lty=2)

# draw ellipse x1 x3
xbar13=c(xbar[1],xbar[3])
S13=matrix(c(S[1,1],S[3,1],S[1,3],S[3,3]),2,2)

eli = ellipse(S13, centre=xbar13,t=sqrt(c2/n), npoint=5000)

plot(eli, cex=.3,bty="n",xlim=c(500,550),ylim=c(23,27), 
     xlab="Social science", ylab="Science",
     type="l",lwd=2,col="blue")

segments(xl1,23-.3,xl1,xl3+.5,lty=2)
segments(xu1,23-.3,xu1,xu3-.5,lty=2)
segments(500-2,xl3,xl1+7,xl3,lty=2)
segments(500-2,xu3,xu1-7,xu3,lty=2)

# draw ellipse x2 x3 This is ellipse calculated in  Example 5-5
xbar23=c(xbar[2],xbar[3])
S23=matrix(c(S[2,2],S[3,2],S[2,3],S[3,3]),2,2)

eli = ellipse(S23, centre=xbar23,t=sqrt(c2/n), npoint=5000)

plot(eli, cex=.3,bty="n",xlim=c(50,60),ylim=c(23,27), 
     xlab="Verbal", ylab="Science",
     type="l",lwd=2,col="blue")

segments(xl2,23-.3,xl2,xl3+.7,lty=2)
segments(xu2,23-.3,xu2,xu3-.7,lty=2)
segments(50-.5,xl3,xl2+1.5,xl3,lty=2)
segments(50-.5,xu3,xu2-1.5,xu3,lty=2)


# Examp 5.6 Construct Bonferroni simultaneous CI and compare to t2--------
# microwave data

# requires library ellipse to draw ellipse
library(ellipse)
radC=read.table("Data/T4-1.dat")
radO=read.table("Data/T4-5.DAT")
radcl=radC**(.25)
radop=radO**(.25)
rad=cbind(radcl,radop)

n=nrow(rad)
p=ncol(rad)
c2=p*(n-1)*qf(.95,p,n-p)/((n-p)*n)
xbar=colMeans(rad)
S=cov(rad)

# for door closed
xl1=xbar[1]-sqrt(c2*mS[1,1])
xu1=xbar[1]+sqrt(c2*mS[1,1])
c(xl1,xu1)

# for door open
xl2=xbar[2]-sqrt(c2*mS[2,2])
xu2=xbar[2]+sqrt(c2*mS[2,2])
c(xl2,xu2)

# Bonferonni
xbonl1=xbar[1]-qt(1-.05/(2*p),n-1)*sqrt(mS[1,1]/n)
xbonu1=xbar[1]+qt(1-.05/(2*p),n-1)*sqrt(mS[1,1]/n)
c(xbonl1,xbonu1)
xbonl2=xbar[2]-qt(1-.05/(2*p),n-1)*sqrt(mS[2,2]/n)
xbonu2=xbar[2]+qt(1-.05/(2*p),n-1)*sqrt(mS[2,2]/n)
c(xbonl2,xbonu2)


eli = ellipse(S, centre=xbar,t=sqrt(c2), npoint=5000)

plot(eli, cex=.3,bty="n", 
     xlab="Radiation door closed-fourth root", 
     ylab="Radiation door open-fourth root",
     xlim=c(.5,.7),ylim=c(.5,.7),type="l",lwd=2,col="blue")

# not shown somehow
segments(xl1,.45,xl1,xu2,lty=3)
segments(xu1,.45,xu1,xu2-.01,lty=3)
segments(.45,xl2,xu1,xl2,lty=3)
segments(.45,xu2,xu1-.01,xu2,lty=3)

segments(xbonl1,.45,xbonl1,xbonu2,lty=2)
segments(xbonu1,.45,xbonu1,xbonu2,lty=2)
segments(.45,xbonl2,xbonu1,xbonl2,lty=2)
segments(.45,xbonu2,xbonu1,xbonu2,lty=2)


# Example 5.7 Construct large sample simultaneous CI ----------------------

yalph=.10
p=7
n=96
xbar=c(28.1,26.6,35.4,34.2,23.6,22.0,22.7)
xsd=c(5.76,5.85,3.82,5.12,3.76,3.93,4.03)

# use qchisq stats
xl=xbar-xsd*sqrt(qchisq(1-yalph,p)/n)
xu=xbar+xsd*sqrt(qchisq(1-yalph,p)/n) 

for (i in 1:p) {
  cat("the CI: ", round(xl[i], 1), " ", round(xu[i], 1), "\n")
}


# Example 5.8 Creating a univariate control chart -------------------------

police=read.table("Data/T5-8.dat")
names(police)=c("legal","Extra","holdo","coa","meeting")
period=c(1:16)
cbind(period,police[,1])

plot(period,police[,1],type="l",xlab="Period number",
     col="blue",cex=1.5,lwd=1.7, 
     ylab="Legal appearances(hours)"
     ,xlim=c(0,20),ylim=c(1500,5500))
points(period,police[,1],pch=19,col="blue")
abline(h=mean(police[,1]))
abline(h=mean(police[,1]) +3*sqrt(var(police[,1])),lty=2)
abline(h=mean(police[,1])-3*sqrt(var(police[,1])),lty=2)
text(c(19,19,19),c(mean(police[,1]) -3*sqrt(var(police[,1]))+150,
                   mean(police[,1])+150,
                   mean(police[,1]) +3*sqrt(var(police[,1]))-150),
                   c("LCL=1737","xbar=3558","UCL=5378"),cex=0.9)


# Example 5.9 An ellipse format chart for overtime hours ------------------

police=read.table("Data/T5-8.dat")
S=matrix(c(var(police[,1]), cov(police[,1],police[,2]),
           cov(police[,1],police[,2]),var(police[,2])),2,2)
# quality ellipse
eli = ellipse(S, centre=c(mean(police[,1]),
                          mean(police[,2])),
              t=sqrt(qchisq(.99,2)), npoint=5000)
plot(eli, cex=.3, 
     xlab="Legal appearances (hours)", 
     ylab="Extraordinary event (hours)",
     xlim=c(1500, 5500),ylim=c(-3000,5500),type="l",lty=1.5)

points(police[,1],police[,2],pch=19,col="blue")
points(mean(police[,1]),mean(police[,2]),pch=3)


# Figure 5.7 X-bar chart for extraordinary event hours.
period=c(1:16)
cbind(period, police[,2])
plot(period,police[,2],type="l",
     xlab="Period number",col="blue",
     cex=1.5,lwd=1.7, 
     ylab="extraordinary event (hours)",
     xlim=c(0,19),ylim=c(-3000,6000))

points(period,police[,2],pch=19,col="blue")
abline(h=mean(police[,2]))
abline(h=mean(police[,2])-3*sqrt(var(police[,2])),lty=2)
abline(h=mean(police[,2])+3*sqrt(var(police[,2])),lty=2)
text(c(18,18,18),c(mean(police[,2])-3*sqrt(var(police[,2]))+200,
                   mean(police[,2])+200,
                   mean(police[,2])+3*sqrt(var(police[,2]))-200),
                   c(expression(LCL==- 2071),
                   expression(bar(x)==1478), 
                   expression(UCL==5027)),cex=0.9)


# Example 5.10 A Tsq chart for overtime hours -----------------------------

# T2 plot for overtime hours. Figure 5.8

police=read.table("Data/T5-8.dat")
n=length(police[,1])

# subtract col means
policec<-apply(police,2,scale,scale=F,center=T) # center only
period=c(1:n)
tsq=diag(policec%*%solve(cov(police))%*%t(policec)) # get T2

# 11th point is an outlier
plot(period,tsq,xlab="observation number",bty="n",
     xlim=c(0,16),ylab=expression(T^2),ylim=c(0,12),type="l",
     main="Legal appearances and extraordinary event hours")
abline(h=qchisq(.99,2)+.4,lty=2)
points(period,tsq,pch=19,col="blue")


# Example 5.11 Control of robotic welder - more then Tsq needed -----------
# very good example, practical purpose
# need to install package ellipse for graphs below
# T2 plot for natural logs of welding data--need to fix

weld=read.table("Data/T5-9.dat")
names(weld)=c("voltage","current","speed","flow")
head(weld)

n=length(weld[,1])
lweld4=log(weld[,4])
lweld=cbind(weld[,1:3],lweld4)

# subtract col means
lweldc<-apply(lweld,2,scale,scale=F,center=T)
case=c(1:n)
tsq=diag(lweldc%*%solve(cov(lweld))%*%t(lweldc))
plot(case,tsq,xlab="case",bty="n",xlim=c(0,45),
     ylab=expression(T^2),ylim=c(0,15),type="l")
abline(h=qchisq(.99,4),lty=4)
points(case,tsq,pch=19,col="blue")
text(c(43,43),c(qchisq(.95,4)+.4,qchisq(.99,4)+.4),
     c("95% Limit","99% Limit"),cex=0.9)
abline(h=qchisq(.95,4),lty=2)

# Figure 5.10 ellipse chart for ln(flow) and ln(voltage)

library(ellipse)

weld=read.table("Data/T5-9.dat")
weld[,5]=log(weld[,4])
wel=cbind(weld[,1],weld[,5])
S=matrix(c(var(weld[,1]),cov(weld[,1],weld[,5]),
           cov(weld[,1],weld[,5]),var(weld[,5])),2,2)

eli = ellipse(S, centre=c(mean(weld[,1]),mean(weld[,5])),
              t=sqrt(qchisq(.99,2)), npoint=5000)
plot(eli, cex=.3, xlab="Voltage", ylab="ln(gas flow)",  xlim=c(20, 24),
     ylim=c(3.85,4.05),type="l",lty=1.5)

points(weld[,1],weld[,5],pch=19,col="blue")
points(mean(weld[,1]),mean(weld[,5]),pch=3)

# Figure 5.11 X-bar chart for ln (gas flow)
case=c(1:40)
lngf=log(weld[,4])
cbind(case,lngf)
plot(case,lngf,type="l",xlab="Case",col="blue",cex=1.5,lwd=1.7, 
     ylab="ln (gas flow )" ,
     xlim=c(0,45),ylim=c(3.89,4.01))
points(case,lngf,pch=19,col="blue")
abline(h=mean(lngf))
abline(h=mean(lngf) +3*sqrt(var(lngf)),lty=2)
abline(h=mean(lngf)-3*sqrt(var(lngf)),lty=2)
text(c(43,43.3,43),
     c(mean(lngf) -3*sqrt(var(lngf))+.003,mean(lngf)+.003,
       mean(lngf) +3*sqrt(var(lngf))-.003),
     c("LCL=3.896","Mean=3.951","UCL=4.005"),cex=0.9)


# Example 5.12 A conrol ellipse for future overtime -----------------------
# remove point 11
# Figure 5.12
library(ellipse)

pol=read.table("Data/T5-8.dat")
police=pol[-11,1:5]
S=matrix(c(var(police[,1]),cov(police[,1],police[,2]),
           cov(police[,1],police[,2]),var(police[,2])),2,2)

# quality ellipse
n=length(police[,1])
p=2
c2=2*(n^2-1)*qf(.95,2,n-2)/(n*(n-2))
eli = ellipse(S,centre=c(mean(police[,1]),mean(police[,2])),
              t=sqrt(c2),npoint=5000)
plot(eli, cex=.3, xlab="legal appearances (hours)", 
     ylab="Extraordinary event (hours)",
     xlim=c(1500, 5500),ylim=c(-700,3000),type="l",lty=1.5)
points(police[,1],police[,2],pch=19,col="blue")
points(mean(police[,1]),mean(police[,2]),pch=3)


# Example 5.13 Illustrating the EM algorithm ------------------------------

# where there are missing data

# The calculation is iterative but is easy when you install the package norm which
# includes software to compute the missing values and the parameter estimates
# It chooses possibly different starting values
library("norm")

# enter the data
x = matrix(c(NA,7,5,NA,0,2,1,NA,3,6,2,5),4,3)
x
# prelim.norm changes the coding of missing values
s=prelim.norm(x)
mlethet=em.norm(s,criterion=0.0001)
mlethet=getparam.norm(s,mlethet,corr=F) #get the estimates on original scale
mlethet # page 255
