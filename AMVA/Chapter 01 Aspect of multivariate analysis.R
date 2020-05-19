
# Ex01.1 a data array -----------------------------------------------------

# enter  data column by column as a vector
X <- matrix(c(42,52,48,58,4,5,4,3), nrow = 4, ncol = 2)

# display it
X


# Ex 1.2 The arrays xbar, Sn and R for bivirate data ----------------------

#first enter  data then calculate using sum command
x1 <- c(42,52,48,58)
x2 <- c(4,5,4,3)
n <- length(x1)
xbar1 <- sum(x1)/n
xbar2 <- sum(x2)/n
c(xbar1,xbar2) #display means

# variance
s11 <- sum((x1-xbar1)^2)/n
s22 <- sum((x2-xbar2)^2)/n

# covariance
s12 <- sum((x1-xbar1)*(x2-xbar2))/n

Sn <- matrix( c(s11,s12,s12,s22),nrow=2,ncol=2) 
Sn #display Sn


# Ex1.4 A scatter plot for baseball data ----------------------------------
baseb=read.table("Data/T1-1.dat")
names(baseb)=c("Payroll","One-lost percent")
plot(baseb,pch=19)   
plot(baseb[,1]*10^{-6},baseb[,2],pch=19)



# Ex1.5 Multiple scatter plots for paper strength measurements ------------

paper=read.table("Data/T1-2.dat")
names(paper)=c("Density","Strength MD","Strength CD")

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,  ...)
}

pairs(paper,pch=16,diag.panel=panel.hist, cex.labels = 2, font.labels=2)

##try boxplot

panel.box <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- boxplot(x, plot = FALSE,...)
}

pairs(paper,pch=16,diag.panel=panel.box, cex.labels = 2, font.labels=2) 


# Ex1.6 Looking for lower-dimenional structure ----------------------------

#need to install package scatterplot3d
library(scatterplot3d)

lizz=read.table("Data/T1-3.dat")
lizz
names(lizz)=c("Mass","SVL","HLS")
with(lizz, {
  scatterplot3d(SVL,HLS,Mass,pch=16,scale.y=.75,grid=F)
})

#To plot the standardized values
n=nrow(lizz)
p=ncol(lizz)
m_x=colMeans(lizz,dim=1) # column means

s_x=sqrt((n-1)/n)*apply(lizz, 2, sd);#column sd using divisor n

Z=lizz
for (i in c(1:p)) {
  Z[,i]=(lizz[,i]-m_x[i])/s_x[i]
  }

names(Z)=c("Mass","SVL","HLS")

with ( Z, {
  scatterplot3d(SVL,HLS,Mass,pch=16,grid=F) })

# Figure 1.8. Need to install package scatterplot3d -----------------------

library(scatterplot3d)
lizz=read.table("Data/T1-3.dat")
augmliz=data.frame(lizz,c("f","m","f","f","m","f","m","f","m","f",
                          "m","f","m","m","m", "m","f","m","m","m",
                          "f","f","m","f","f"))
names(augmliz)=c("Mass","SVL","HLS","gender")

with(augmliz, {
  scatterplot3d(SVL,HLS,Mass,
                color=c("red","blue")[as.numeric(augmliz$gender)],
                pch=16,grid=F)
})

#You can add a legend using the legend function:

legend(x="topleft", legend = levels(augmliz$gender), 
       col=c("red","blue"), 
       pch=16)

# Ex1.9 rotated plots in three dimensions ---------------------------------

#You need to install the package rgl. Then it is easy to rotate with mouse
library(rgl)
datpap=read.table("Data/T1-2.dat")
dpap=datpap[,1:3]
names(dpap)=c("x1","x2","x3")
with(dpap, {plot3d(x1,x2,x3,box=FALSE,axes=F,type="s",size=1.2,col="blue")
  abclines3d(mean(x1),mean(x2),mean(x3),a=diag(3))
  decorate3d(box=F,axes=F,xlab="x1",ylab="x2",zlab="x3")}) 
#add axes centered at means
#hold down the left button on the mouse and move mouse to rotate figure


# Ex1.10 Arrays of growth curves ------------------------------------------

#Growth curves weight all bears
fbear=read.table("Data/T1-4.dat")
head(fbear)
wtbear=fbear[,1:4]
wt=t(wtbear)
x=c(2,3,4,5)

plot(wt, xlim=c(1,4),ylim=c(0,250),axes=F,xlab="Year",ylab="Weight",type="l")
lines(wt[,1],type="l",lty=1)
lines(wt[,2],type="l",lty=2)
lines(wt[,3], type="l",lty=3)
lines(wt[,4], type="l",lty=4)
lines(wt[,5], type="l",lty=5)
lines(wt[,6], type="l",lty=6)
lines(wt[,7], type="l",lty=7)
axis(1, at=1:4, lab=c("2","3","4","5"))
axis(2, las=1, at=50*0:5,lab=c("0","50","100","150","200","250"))
lab=c("1","2","3","4","5","6","7")
text(c(2,2,2,2,2.5,2.5,2.5),c(60,68,77,43,165,91,102),lab)

#Growth curves weight,  Figure 1.14.

fbear=read.table("Data/T1-4.DAT")
wtbear=fbear[,1:4]
wt=t(wtbear)
x=c(2,3,4,5)
par(mfrow=c(2,4))
plot(x,wt[,1], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 1")
plot(x,wt[,2], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 2")
plot(x,wt[,3], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 3")
plot(x,wt[,4], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 4")
#correct weight to kilograms
wt[,5]=c(45,66,84,112)
plot(x,wt[,5], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 5")
plot(x,wt[,6], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 6")
plot(x,wt[,7], xlim=c(1,5),ylim=c(0,150),xlab="Year",ylab="Weight",type="l",main="Bear 7")


#Figure 1-15 Growth curves of length

fbear=read.table("Data/T1-4.DAT")
lngbear=fbear[,5:8]
lng=t(lngbear)
x=c(2,3,4,5)
par(mfrow=c(2,4))
plot(x,lng[,1], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l")
plot(x,lng[,2], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l")
plot(x,lng[,3], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l")
plot(x,lng[,4], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l")
plot(x,lng[,5], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l")
plot(x,lng[,6], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l")
plot(x,lng[,7], xlim=c(1,5),ylim=c(140,190),xlab="Year",ylab="Length",type="l") 

dev.off()
# Ex1.11 Utility data as stars --------------------------------------------

X=read.table("Data/T12-4.dat")
names(X)=c("x1","x2","x3","x4","x5","x6","x7","x8","company")

#select the first five rows of the data.
fiveutil=rbind(X[1,],X[2,],X[3,],X[4,],X[5,])
compnam=c("Arizona","Boston","Central", "Commonwealth", "Consolidated")
stars(fiveutil,labels=compnam)


# Ex1.12 Utility data as Chernoff faces -----------------------------------


#Chernott faces requires special software  package aplpack. Install using 
install.packages("aplpack")
# The part of the package that creates Chernoff faces  is called faces.
#This newer version of faces also employs color so we do not get a direct 
# comparison with the black and white version.


library(aplpack)
utilities=read.table("Data/T12-4.dat")
names(utilities)=c("x1","x2","x3","x4","x5","x6","x7","x8","company")
head(utilities)

faces(utilities[,1:8], labels=utilities$company)
# instead of default assignment of features, we assign x3= width of mouth, 
# x4= height of eyes, and x5=shape of face. The original choices are no 
# longer available. Columns of constants must be created for variables not 
# used. 

#Create a data.frame for plotting.
cherutil=matrix(1,nrow=22,ncol=15) # all constant
cherutil[,2]=utilities$x2
cherutil[,1]=utilities$x1
cherutil[,3]=utilities$x5
cherutil[,5]=utilities$x3
cherutil[,7]= utilities$x4
cherutil[,8]=utilities$x6
cherutil[,6]=utilities$x7
cherutil[,12]=utilities$x8
#rownames(cherutil)
compnam=c("Arizona","Boston","Central", "Common", "Consolid",
          "Florida", "Hawaiian", "Idaho", "Kentucky","Madison", "Nevada", "NewEngla",
          "Northern", "Oklahoma", "Pacific", "Puget", "SanDiego", "Southern", "Texas","Wisconsi", "United",
          "Virginia")
faces(cherutil,face.type=0,labels=compnam)

#dropping face.type=0  gives  faces that are colored
