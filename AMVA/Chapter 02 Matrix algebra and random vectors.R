# Chapter 02 Matrix algebra and random vectors 

# page 53 /6ed
# Example 2.1 Calculating length of vectors and the angle between  --------

# Enter the vectors
x=c(1,3,2)
y=c(-2,1,-1)
prod=3*x
sum=x+y
c(prod,sum)   #check

# t(x) is the transpose and 
# %*%  is matrix multiplication
innerp=t(x)%*%y
innerp
class(innerp) # "matrix" "array" 

c(sum,innerp) #check

# length
Lx=sqrt(t(x)%*%x)
Ly=sqrt(t(y)%*%y)
c(Lx,Ly)

xycos=t(x)%*%y/(Lx*Ly)
xycos

(180/pi)*acos(t(x)%*%y/(Lx*Ly))   #angle in degrees 96.3 deg
L3x=sqrt(t(3*x)%*%(3*x)) 
L3x
L3x / Lx  # L3x = 3Lx


# Ex2.3 the transpose of a matrix -----------------------------------------

A=matrix(c(3,1,-1,5,2,4),2,3)
A
t(A)


# Ex2.4 The sum of two matrix and multiplication of matrix by cons --------

A=matrix(c(0,1,3,-1,1,1),2,3) # fill the column first
A
B=matrix(c(1,2,-2,5,-3,1),2,3)
C=4*A
C
D=A+B
D


# Ex2.5 matrix mutiplication ----------------------------------------------

A=matrix(c(3,1,-1,5,2,4),2,3)
B=c(-2,7,9)
C=matrix(c(2,1,0,-1),2,2)

# then the products are
D=A%*%B
D

# pay attention to the order
E=C%*%A 
E


# Ex2.6 some typical products and their dimensions ------------------------

A=matrix(c(1,2,-2,4,3,-1),2,3)
b=c(7,-3,6) # it is a colunm by default
c=c(5,8,-4)
d=c(2,9)

#recall that you need to enter prod 1 etc to see product
(prod1=A%*%b)             
(prod2=t(b)%*%c)
(prod3=b%*%t(c))
(prod4=t(d)%*%A%*%b)


# Ex 2.7 a symmetirc matrix -----------------------------------------------

A=matrix(c(3,4,6,-2),2,2)
A
# check that A-A' not equal 0 matrix

A-t(A)

B <- matrix(c(3, 5, 5, -2), 2, 2)
B
B - t(B)


# Ex2.8 the existence of matrix inverse -----------------------------------
A=matrix(c(3,4,2,1),2,2)
solve(A)

B <- matrix(c(-0.2, 0.8, 0.4, -0.6), 2, 2)
round(B %*% A, 3)


# Ex2.9 verifying eigenvalues and eigenvectors ----------------------------
A=matrix(c(1,-5,-5,1),2,2)
eigen(A)

# In the text, we choose the negative of second eigenvector

x = eigen(A)
x[1]
x[2]
x$vectors[,1]

# verify 1 = t(e)*e...
t(x$vectors[,1]) %*% x$vectors[,1]


# Ex 2.10 the spectral decomposition of a matrix --------------------------
# page 62 
A=matrix(c(13,-4,2,-4,13,-2,2,-2,10),3,3)
A
eigen(A)
# Because two eigenvalues equal 9, we will learn that the choice of
# the corresponding eigenvectors is not unique 
e1 = eigen(A)$vectors[, 1]
e2 = eigen(A)$vectors[, 2]
e3 = eigen(A)$vectors[, 3]

AA = 18*e1%*%t(e1) + 9*e2%*%t(e2) + 9*e3%*%t(e3)
AA # verified ! - page 62


# Ex 2.11 a positive defnite matrix and quadratic form --------------------

# the matrix is symmetric and has positive eigenvalues
A=matrix(c(3,-sqrt(2),-sqrt(2),2),2,2)
eigen(A)


# Ex 2.12 computing expected values for discrete random variables ---------

x=c(-1,0,1)
xp=c(.3,.3,.4)
xmu=crossprod(x,xp) # enter xmu to see result
xmu
# same result / concept
xmu2 <- sum(x*xp)
xmu2

y=c(0,1)
yp=c(.8,.2)
ymu=crossprod(y,yp)
ymu


# Ex2.13 compute the covariance matrix ------------------------------------

# From Example 2-12 we already have the means.
x=c(-1,0,1)
xp=c(.3,.3,.4)
xmu=crossprod(x,xp)
y=c(0,1)
yp=c(.8,.2)
ymu=crossprod(y,yp)

# To calculate the variances, we again use the marginal probabilities
sigma11=crossprod((x-xmu)^2,xp)
sigma22=crossprod((y-ymu)^2,yp)

# use joint distribution
P=matrix(c(.24,.16,.4,.06,.14,0),3,2)
B=P%*%(y-ymu)

sigma12=crossprod(x-xmu,B) 
c(sigma11,sigma22,sigma12)


# Ex 2.14 computing the correlation matrix from the covariance mat --------

A=matrix(c(4,1,2,1,9,-3,2,-3,25),3,3)
V=diag(diag(A))
Vhalf=sqrt(V)
solve(Vhalf)

# correlation matrix is
solve(Vhalf)%*%A%*%solve(Vhalf) 
