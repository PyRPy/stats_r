# Chapter 03 Sample geometry and random sampling

# Example 3.1 computing the mean vector -----------------------------------

X=matrix(c(4,-1,3,1,3,5),3,2)
xbar=colMeans(X)
XA=rbind(X,xbar)
XA

#include
plot(XA[,1],XA[,2],
     xlim=c(-1,4.5),ylim=c(0,5.5),
     pch=19,col="blue", cex=1.2,bty="n",xlab="",ylab="")
points(xbar[1],xbar[2],pch=1,cex=2.5)
#label points
text(-1,3.25,expression(italic(x)[2]))
text(3.25,5,expression(italic(x)[3]))
text(4.25,1,expression(italic(x)[1]))
text(2.25,3,expression(bar(italic(x))))


# Example 3.2 data as p vectors in n dimensions ---------------------------
# not very meaningful
# The graph requires library rgl
library(rgl)
# plot vectors as dark blue lines. Hold down left mouse button and
# move mouse to rotate to orientate axes or look a plane of vectors
open3d()
plot3d(box=F,axes=F)
decorate3d(xlim=c(-2,5),ylim=c(-2,5),z=c(-2,5),
           xlab="1",ylab="2",zlab="3",box=F,axes=F)
abclines3d(0,0,0,a=diag(8))
rgl.lines(c(0,4),c(0,-1),c(0,3),lwd=2,col="blue")
rgl.lines(c(0,5),c(0,3),c(0,5),lwd=2,col="blue")



# Example 3.3 decomposing a vector into its mean and deviation com --------

# The graph requires package rgl
# enter data in matrix X.  See C1E1-1r.tex
X=matrix(c(4,-1,3,1,3,5),nrow=3,ncol=2)
n=nrow(X)
p=ncol(X)
y1=X[,1] # y1 and y2 are columns in matrix
y2=X[,2]
xbar1=mean(y1)
xbar2=mean(y2)

# create a matrix with n rows of xbar
one=matrix(c(rep(1,n)), nrow=n,ncol=1)
one

# the second  part of  decomposition
xbar1*one
xbar2*one
(d1=y1-xbar1*one)
(d2=y2-xbar2*one)

t(xbar1*one)%*%(y1-xbar1*one)     # = 0 check
t(xbar2*one)%*%(y2-xbar2*one)

# the two decompositions are
y1=xbar1*one+(y1-xbar1*one)
y2=xbar2*one+(y2-xbar1*one)


# Example 3.4 Calculating Sn and R from deviation vectors -----------------

# enter data in matrix X.  See C1E1-1r.tex
X=matrix(c(4,-1,3,1,3,5),nrow=3,ncol=2)
n=nrow(X)
p=ncol(X)
xbar=colMeans(X)

# create a matrix with n rows of xbar
one=matrix(c(rep(1,n)), nrow=n,ncol=1)
Dev=X-one%*%t(xbar)
Dev

# The three products are in the matrix
t(Dev)%*%Dev

# covariance matrix with divisor n
Sn=t(X-one%*%t(xbar))%*% (X-one%*%t(xbar))/n
Sn

r=Sn[1,2]/(sqrt(Sn[1,1]*Sn[2,2]))
r 
# The graph of the deviation vectors is obtained in E3-3r 


# Example 3.6 a nonrandom sample ------------------------------------------

dwaste = read.table("Data/T3-1.dat")
names(dwaste) = c("paper", "plastic")
year = c(1960, 1970, 1980, 1990, 1995, 2000, 2003)
dwaste <- cbind(year, dwaste)

with(data = dwaste, 
plot(year,paper,col="blue",pch=1),
)

with(data = dwaste,
     lines(year,plastic, col="green"))

# the trends are very strong 


# Example 3.7 calculating a generalized variance --------------------------

S= matrix(c(252.04,-68.43,-68.43,123.67),2,2)

det(S) # related to surface area...


# Example 3.8 interpreting the generalized variance -----------------------

# enter the three covariance matrices and obtain eign vectors and values
S1=matrix(c(5,4,4,5),2,2)
S2=matrix(c(3,0,0,3),2,2)
S3=matrix(c(5,-4,-4,5),2,2)

# same generalized variance
det(S1)
det(S2)
det(S3)
eig1=eigen(S1)
eig1
eig2=eigen(S2)
eig2
eig3=eigen(S3)
eig3
c2=qchisq(.95,2)

# the first ellipse extends from the mean vector, in each direction
sqrt(c2)*sqrt(eig1$value[1])  #in the direction of the first eigenvector
sqrt(c2)*sqrt(eig1$value[2])   #in the direction of the second eigenvector

# the distances for the other two cases are
sqrt(c2)*sqrt(eig2$value[1])
sqrt(c2)*sqrt(eig2$value[2])
sqrt(c2)*sqrt(eig3$value[1])
sqrt(c2)*sqrt(eig3$value[2]) 


# Example 3.9 a case where the generalized variance is zero ---------------

# The plot requires package rgl
library(rgl)
X=matrix(c(1,4,4,2,1,0,5,6,4),3,3)
S=cov(X)
S
det(S) # det(s) = 0

# We plot the three column vectors as blue lines. Hold down the left mouse 
# bottom and move mouse to look at the plane of the 3 vectors. This graph 
# requires library rgl
plot3d(box=F,axes=F)
decorate3d(xlim=c(-2,5),ylim=c(-2,5),z=c(-2,5),
           xlab="1",ylab="2",zlab="3",box=F,axes=F)
abclines3d(0,0,0,a=diag(36))
rgl.lines(c(0,1),c(0,4),c(0,4),lwd=2,col="blue")
rgl.lines(c(0,2),c(0,1),c(0,0),lwd=2,col="blue")
rgl.lines(c(0,5),c(0,6),c(0,4),lwd=2,col="blue")


# Example 3.10 creating new variables that lead to a zero generali --------
# kind of like 3-D visulization
X=matrix(c(1,4,2,5,3,9,12,10,8,11,10,16,12,13,14),5,3)
X
S=cov(X)
det(S)
eig=eigen(S)
eig
# Note that  the third  eigenvalue is essentially 0
# with numerical error although the exact value is 0
# Calculate  the deviation vector
one=matrix(rep(1,5),5,1)
one
dev=X-one%*%colMeans(X) # dont' forget %*% is a mattrix * matrix operator
dev
# verify the linear combination is a zero vector
dev%*%eig$vector[,3]
# Numerically, round-off errors give very small values but not 0 


# Example 3.11 illustrating the relation between |S| and |R| --------------

S = matrix(c(4,3,1,3,9,2,1,2,1),3,3)
R=cov2cor(S)     #convert cov to cor matrix
det(R)
det(S)

#verify difference is essentially 0
round(det(S)-S[1,1]*S[2,2]*S[3,3]*det(R), 10)


# Example 3.12 calculating the total sample variance ----------------------
# from ex3.7
S=matrix(c(252.04,-68.43,-68.43,123.67),2,2)
S
S[1,1]+S[2,2]

# from ex3.9
S1=matrix(c(3,-1.5,0,-3.5,1,.5,0,.5,1),3,3)
S1[1,1]+S1[2,2]+S1[3,3]
# The trace of a square matrix is the sum of diagonal elements


# Example 3.13 means and covariances for linear combination ---------------

X=matrix(c(1,4,4,2,1,0,5,6,4),3,3)
b=c(2,2,-1)
c=c(1,-1,3)

# the three  values for the  first linear combinations are
y1=X%*%b
mean(y1)
var(y1)
y2=X%*%c
mean(y2)
var(y2)  

S=matrix(c(3,-1.5,0,-1.5,1,.5,0,.5,1),3,3)
xbar=c(3,1,5)
# We check that
t(b)%*%xbar
t(c)%*%xbar
t(b)%*%S%*%b
t(c)%*%S%*%c
t(b)%*%S%*%c 
