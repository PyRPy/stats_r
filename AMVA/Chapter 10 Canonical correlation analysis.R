# Chapter 10 Canonical correlation analysis

# Example 10.1 Calculate canonical variates -------------------------------

Rho=matrix(c(1,.4,.5,.6,.4,1,.3,.4,.5,.3,1,.2,.6,.4,.2,1),4,4)
R11=Rho[1:2,1:2]
R22=Rho[3:4,3:4]
R12=Rho[1:2,3:4]
R21=t(R12)

# define negative half power of matrix
"%^%" <- function(x, n) with(eigen(x), vectors %*% (values^n * t(vectors)))

R11%^%(-0.5)
solve(R22)
R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) )

# we reverse sign of first eigenvector 

a1=R11%^%(-.5)%*% (-eig$vector[,1])  

# the coefficients are
eig=eigen( R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) ))   
b1= drop((sqrt(t(a1)%*%R12%*%solve(R22)%*%R21%*%a1))^(-1))*solve(R22)%*%R21%*%a1

# the canonial correlations are
rhocc1=sqrt(eig$value[1])
rhocc2=sqrt(eig$value[2])
c(rhocc1,rhocc2)
# the correlation of simple variables directly from second moments
a=c(3,1)
b=c(1,1)
covU1V1=t(a)%*%R12%*%b
varU1=t(a)%*%R11%*%a
varV1=t(b)%*%R22%*%b
c(varU1,varV1,covU1V1)
corU1V1=covU1V1/(sqrt(varU1*varV1)) 
corU1V1


# Example 10.2 Compute correlations between canonical variates ------------

# enter the correlations from Example 10.1
Rho=matrix(c(1,.4,.5,.6,.4,1,.3,.4,.5,.3,1,.2,.6,.4,.2,1),4,4)
R11=Rho[1:2,1:2]
R22=Rho[3:4,3:4]
R12=Rho[1:2,3:4]
R21=t(R12)

#  define negative half power of matrix
"%^%" <- function(x, n) with(eigen(x), vectors %*% (values^n * t(vectors)))
R11%^%(-0.5)
solve(R22)
R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) )
eig=eigen( R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) ))   
# we reverse sign of first eigenvector 

a1=R11%^%(-.5)%*% (-eig$vector[,1])  

#the coefficients are
b1= drop((sqrt(t(a1)%*%R12%*%solve(R22)%*%R21%*%a1))^(-1))*solve(R22)%*%R21%*%a1 
A=t(a1)
B=t(b1)
rhoU1z1=round(A,2)%*%R11
rhoV1z2=round(B,2)%*%R22 
rhoU1z1
rhoV1z2 
rhoU1z2=round(A,2)%*%R12
rhoV1z1=round(B,2)%*%R21
rhoU1z2
rhoV1z1


# Example 10.4 Canonical correlation analysis of chicken-bone data --------

R=matrix(scan("Data/E9-14rcor.dat"),6,6)

R11=R[1:2,1:2]
R22=R[3:4,3:4]
R12=R[1:2,3:4]
R21=t(R12)

# define negative half power of matrix
"%^%" <- function(x, n) with(eigen(x), vectors %*% (values^n * t(vectors)))
R11%^%(-0.5)
solve(R22)
R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) )
eig1=eigen( R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) ))   
# below we reverse sign of first eigenvector
ccor=sqrt(eig1$value)
ccor   #canonical correlations
eig2=eigen( R22%^%(-0.5)%*%R21%*%solve(R11)%*%R12%*%(R11%^%(-0.5) ))
# the coefficients of the canonical variables are
A=t(R11%^%(-.5)%*% (-eig1$vector[,1:2]))
B= t(R22%^%(-.5)%*% (-eig2$vector[,1:2]))  #sign reversed for V2

# Alternatively, when the observations themselves are available
# we could proceed as follows after installing package CCA

library(CCA)
datc=read.table("Data/E9-14.dat")
names(datc)=c("skullen","skullbr","femurlen","tibialen","humerlen","ulnalen")
dat=scale(datc)
head=dat[,1:2]
leg=dat[,3:4]
ccl=cc( head,leg)

# canonical corr
ccl$cor

# coefficients
ccl[3:4]

# corelations with original variables(loadings)
cc2=comput(head,leg,ccl)
cc2 


# Example 10.5 Job satisfaction -------------------------------------------

R=matrix(scan("Data/E10-5.dat"),12,12)
R22=R[6:12,6:12]
R11=R[1:5,1:5]
R21=R[6:12,1:5]
R12=t(R21)

# define negative half power of matrix
"%^%" <- function(x, n) with(eigen(x), vectors %*% (values^n * t(vectors)))
R11%^%(-0.5)
solve(R22)
R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) )
# below we reverse signs of the eigenvectors
eig1=eigen( R11%^%(-0.5)%*%R12%*%solve(R22)%*%R21%*%(R11%^%(-0.5) ))

# below we reverse signs of the eigenvectors
eig2=eigen( R22%^%(-0.5)%*%R21%*%solve(R11)%*%R12%*%(R22%^%(-0.5) ))   
ccor=sqrt(eig1$value)

# the coefficients of the canonical variables are
A=t(R11%^%(-.5)%*% (-eig1$vector[,1:2]))
B= t(R22%^%(-.5)%*% (-eig2$vector[,1:2]))

round(A,2)  #the first two pairs differ slightly from the text
round(B,2)  #Note that the coefficients for V2 are the negative of those in the text.
# solve(A)
# solve(B)

# the correlations of the canonical variables with the original variables

rU1z1=A[1,]%*%R11
rU1z2=A[1,]%*%R12
rV1z2=B[1,]%*%R22
rV1z1=B[1,]%*%R21

rU1z1
rU1z2
rV1z2
rV1z1
