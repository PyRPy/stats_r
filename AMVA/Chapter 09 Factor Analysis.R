# Chapter 09 Factor Analysis

# Example 9.1 Verify covariance matrix - decomposition --------------------

#Enter matrices beginning with covariance matrix
SIG=matrix(c(19,30,2,12,30,57,5,23,2,5,38,47,12,23,47,68),4,4)

L=matrix(c(4,7,-1,1,1,2,6,8),4,2)
Psi=diag(c(2,4,1,3))

# check that LL'+Psi equals Sigma by calculating the difference is a zero matrix
SIG-L%*%t(L)-Psi

# communality for X1
(L%*%t(L))[1,1]

# check equality for sigma11
c(SIG[1,1],(L%*%t(L))[1,1]+Psi[1,1])

