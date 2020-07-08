# Chapter 11 Discriminationa and classification 

# Example 11.1 Discriminate owners from nonowners of mowers ---------------

dat=read.table("Data/T11-1.dat")
colnames(dat)=c("Income", "Lot size","Ownership")

plot(dat[1:12,1],dat[1:12,2], 
     xlab="Income(1000's dollars)",
     ylab="Lot size(sq.ft.)",
     pch=1,
     xlim=c(60,150),
     ylim=c(7,26))

points(dat[13:24,1],dat[13:24,2],pch=19)
lines(c(70,25),c(135,9),type="l")

expression(75,5,R[2])
expression(125,24,R[1]) 


# Example 11.2 Classify a new observation into one of two pop. ------------

# We are given
c12=10
c21=5
p1=.8
p2=.2

# The region R1 consists of x where f1(x)/f2(x) is greater or equal to
(c12/c21)*(p2/p1)

# To classify a new observation x0 where
f1x0=.3
f2x0=.4
# we compare
c(f1x0/f2x0,(c12/c21)*(p2/p1))   #and assign x0 to population 1


# Example 11.3 Classification with two normal populations -----------------


#The authors only give a plot of the data so we base our analysis on
# the summary statistics from their example.
xbar1=c(-.0065,-.0390)
xbar2=c(-.2483,.0262)
Spinv=matrix(c(131.158,-90.423,-90.423,108.147),2,2)
colnames(Spinv)=c("Log10(activity)", "Log10(antigen)")

# the equal cost equal prior rule has coefficients
a=(xbar1-xbar2)%*%Spinv
a
ybar1=a%*%xbar1
ybar2=a%*%xbar2
m=(ybar1+ybar2)/2

x0=c(-.210,-.044)
a%*%x0
c(m,a%*%x0)

# the unequal prior case
p1=.75
p2=.25

# use w
w=a%*%x0-m
c(w,log(p2/p1))


# Example 11.4 Fisher's linear discriminant for hemophilia data -----------

# The summary statistics are
xbar1=c(-.0065,-.0390)
xbar2=c(-.2483,.0262)
Spinv=matrix(c(131.158,-90.423,-90.423,108.147),2,2)
colnames(Spinv)=c("Log10(activity)", "Log10(antigen)")

# the  coefficients
a=(xbar1-xbar2)%*%Spinv
a

# maximal separation is
D2=t(xbar1-xbar2)%*%Spinv%*%(xbar1-xbar2)
D2


# Example 11.7 Estimate of error rate using holdout proc ------------------

# We are given
dat1=matrix(c(2,4,3,12,10,8),3,2)
dat2=matrix(c(5,3,4,7,9,5),3,2)
xbar1=colMeans(dat1)
xbar2=colMeans(dat2)
n1=nrow(dat1)
n2=nrow(dat2)
S1=cov(dat1)
S2=cov(dat2)
Sp=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)

# begin holdout calculation with dat1
xbar1H=colMeans(dat1[2:3,])
SpH=((n1-2)*cov(dat1[2:3,])+(n2-1)*cov(dat2))/(n1+n2-3)
Dsq111=(dat1[1,]-xbar1H)%*%solve(SpH)%*%(dat1[1,]-xbar1H)
Dsq112=(dat1[1,]-xbar2)%*%solve(SpH)%*%(dat1[1,]-xbar2)
c(Dsq111,Dsq112)         #correctly assign  to  pi1

xbar1H=colMeans(rbind(dat1[1,],dat1[3,]))
SpH=((n1-2)*cov(rbind(dat1[1,],dat1[3,]))+(n2-1)*cov(dat2))/(n1+n2-3)
Dsq121=(dat1[2,]-xbar1H)%*%solve(SpH)%*%(dat1[2,]-xbar1H)
Dsq122=(dat1[2,]-xbar2)%*%solve(SpH)%*%(dat1[2,]-xbar2)
c(Dsq121,Dsq122)              #incorrectly assign to pi2

xbar1H=colMeans(dat1[c(1:2),])
SpH=((n1-2)*cov(dat1[c(1:2),])+(n2-1)*cov(dat2))/(n1+n2-3)
Dsq131=(dat1[3,]-xbar1H)%*%solve(SpH)%*%(dat1[3,]-xbar1H)
Dsq132=(dat1[3,]-xbar2)%*%solve(SpH)%*%(dat1[3,]-xbar2)
c(Dsq131,Dsq132)         #incorrectly assign to pi2

# begin holdout calculation with dat2
xbar2H=colMeans(dat2[2:3,])
SpH=((n1-1)*cov(dat1)+(n2-2)*cov(dat2[2:3,]))/(n1+n2-3)
Dsq211=(dat2[1,]-xbar1)%*%solve(SpH)%*%(dat2[1,]-xbar1)
Dsq212=(dat2[1,]-xbar2H)%*%solve(SpH)%*%(dat2[1,]-xbar2H)
c(Dsq211,Dsq212)           #correctly assign to pi2

xbar2H=colMeans(rbind(dat2[1,],dat2[3,]))
SpH=((n1-1)*cov(dat1)+(n2-2)*cov(rbind(dat2[1,],dat2[3,])))/(n1+n2-3)
Dsq221=(dat2[2,]-xbar1)%*%solve(SpH)%*%(dat2[2,]-xbar1)
Dsq222=(dat2[2,]-xbar2H)%*%solve(SpH)%*%(dat2[2,]-xbar2H)
c(Dsq221,Dsq222)          #incorrectly assign to pi1
xbar2H=colMeans(dat2[1:2,])
SpH=((n1-1)*cov(dat1)+(n2-2)*cov(dat2[1:2,]))/(n1+n2-3)
Dsq231=(dat2[3,]-xbar1)%*%solve(SpH)%*%(dat1[3,]-xbar1)
Dsq232=(dat2[3,]-xbar2H)%*%solve(SpH)%*%(dat2[3,]-xbar2H)
c(Dsq231,Dsq232)              #incorrectly assign to pi1

EAER=(2+2)/(3+3)
EAER
# LAST ASSIGN DIFFERENT FROM BOOK 


# Example 11.8 Classify Alaskan and Canadian salmon -----------------------

# need to install package MASS
library(MASS)
salmdat=read.table("Data/T11-2.dat")
names(salmdat)=c("country","gender","fresh","marine")
fit=lda(country~fresh+marine,data=salmdat,CV=T)
fit
# Table the classification results
ct <- table(salmdat$country, fit$class)
diag(prop.table(ct, 1))

# get a confusion matrix table
table(salmdat$country, fit$class)

# total percent correct
sum(diag(prop.table(ct)))

#linear discriminant
xbar1=colMeans(salmdat[1:50,3:4])
S1=cov(salmdat[1:50,3:4])
xbar2=colMeans(salmdat[51:100,3:4])
S2=cov(salmdat[51:100,3:4])
a=(xbar1-xbar2)%*%solve((S1+S2)/2)
.5*(xbar1-xbar2)%*%solve((S1+S2)/2)%*%(xbar1+xbar2)
