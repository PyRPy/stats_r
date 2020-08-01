##############################################
#  R Code for canonical correlation analysis #
##############################################
# http://people.stat.sc.edu/Hitchcock/chapter8_R_examples.txt
# https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/

# We will use the built-in iris data set.
# We will consider the entire data set (all three species)

# Canonical correlation analysis ------------------------------------------

attach(iris)
head(iris)
# We will standardize the variables first
# by dividing by each column's standard deviation:
# (we will remove column 5, the species labels)

iris.std <- sweep(iris[,-5], 2, sqrt(apply(iris[,-5],2,var)), FUN="/")

sepal.meas <- iris.std[,1:2]
petal.meas <- iris.std[,3:4]

### Doing the CCA the long way:

# Finding blocks of the correlation matrix:

R11 <- cor(sepal.meas)
R22 <- cor(petal.meas)
R12 <- c(cor(sepal.meas[,1], petal.meas[,1]), cor(sepal.meas[,1], petal.meas[,2]),
         cor(sepal.meas[,2], petal.meas[,1]), cor(sepal.meas[,2], petal.meas[,2]))
R12 <- matrix(R12, ncol=ncol(R22), byrow=T) # R12 has q2 columns, same as number of petal measurements
R21 <- t(R12)  # R21=transpose of R12

# Finding the E1 and E2 matrices:

E1 <- solve(R11) %*% R12 %*% solve(R22) %*% R21
E2 <- solve(R22) %*% R21 %*% solve(R11) %*% R12

# print(E1)
# print(E2)

eigen(E1)
eigen(E2)

# The canonical correlations are:

canon.corr <- sqrt(eigen(E1)$values)
canon.corr

# The canonical variates are based on the eigenvectors of E1 and E2:

# a1 = (0.922, -0.388)
# b1 = (0.943, -0.333)
# a2 = (0.457, 0.890)
# b2 = (-0.679, 0.734)

# Only the first canonical correlation is really substantial:

# u1 = 0.92*Sepal.Length - 0.39*Sepal.Width
# v1 = 0.94*Petal.Length - 0.33*Petal.Width

# Plotting the first set of canonical variables:

u1 <- as.matrix(iris.std[,1:2]) %*% as.matrix(eigen(E1)$vectors[,1])
v1 <- as.matrix(iris.std[,3:4]) %*% as.matrix(eigen(E2)$vectors[,1])
plot(u1,v1)
cor(u1,v1)

# Plotting the second set of canonical variables:

u2 <- as.matrix(iris.std[,1:2]) %*% as.matrix(eigen(E1)$vectors[,2])
v2 <- as.matrix(iris.std[,3:4]) %*% as.matrix(eigen(E2)$vectors[,2])
plot(u2,v2)
cor(u2,v2)

### Doing CCA using the built-in cancor function:

cancor(sepal.meas, petal.meas)

# The canonical correlations are the same as the ones we found,
# The canonical variates are a little different because the cancor 
# function works with the centered data rather than the original data.


### Doing CCA using Dr. Habing's cancor2 function:

### First copy this function into R: #############
##
cancor2<-function(x,y,dec=4){
  # Canonical Correlation Analysis to mimic SAS PROC CANCOR output.
  # Basic formulas can be found in Chapter 10 of Mardia, Kent, and Bibby (1979).
  # The approximate F statistic is exercise 3.7.6b.
  x<-as.matrix(x);y<-as.matrix(y)
  n<-dim(x)[1];q1<-dim(x)[2];q2<-dim(y)[2];q<-min(q1,q2)
  S11<-cov(x);S12<-cov(x,y);S21<-t(S12);S22<-cov(y)
  E1<-eigen(solve(S11)%*%S12%*%solve(S22)%*%S21);E2<-eigen(solve(S22)%*%S21%*%solve(S11)%*%S12)
  rsquared<-as.double(E1$values[1:q])
  LR<-NULL;pp<-NULL;qq<-NULL;tt<-NULL
  for (i in 1:q){
    LR<-c(LR,prod(1-rsquared[i:q]))
    pp<-c(pp,q1-i+1)
    qq<-c(qq,q2-i+1)
    tt<-c(tt,n-1-i+1)}
  m<-tt-0.5*(pp+qq+1);lambda<-(1/4)*(pp*qq-2);s<-sqrt((pp^2*qq^2-4)/(pp^2+qq^2-5))
  F<-((m*s-2*lambda)/(pp*qq))*((1-LR^(1/s))/LR^(1/s));df1<-pp*qq;df2<-(m*s-2*lambda);pval<-1-pf(F,df1,df2)
  outmat<-round(cbind(sqrt(rsquared),rsquared,LR,F,df1,df2,pval),dec)
  colnames(outmat)=list("R","RSquared","LR","ApproxF","NumDF","DenDF","pvalue")
  rownames(outmat)=as.character(1:q);xrels<-round(cor(x,x%*%E1$vectors)[,1:q],dec)
  colnames(xrels)<-apply(cbind(rep("U",q),as.character(1:q)),1,paste,collapse="")
  yrels<-round(cor(y,y%*%E2$vectors)[,1:q],dec)
  colnames(yrels)<-apply(cbind(rep("V",q),as.character(1:q)),1,paste,collapse="")
  list(Summary=outmat,a.Coefficients=E1$vectors,b.Coefficients=E2$vectors,
       XUCorrelations=xrels,YVCorrelations=yrels)
} 
## END FUNCTION
#################################################

# Then use it as on the iris example:

cancor2(sepal.meas, petal.meas)

# It produces two other pieces of information:  An F-test for the significance of
# each canonical correlation, and the correlations between the original variables
# and the corresponding canonical variates.


# Use CCA library ---------------------------------------------------------
library(CCA)
library(CCP)
matcor(sepal.meas, petal.meas)
cc1 <- cc(sepal.meas, petal.meas)

cc1$cor  # display the canonical correlations
cc1[3:4] # raw canonical coefficients

cc2 <- comput(sepal.meas, petal.meas, cc1) # compute canonical loadings
cc2[3:6] # display canonical loadings

# tests of canonical dimensions
rho <- cc1$cor
# Define number of observations, number of variables in first set, and number 
# of variables in the second set.
n <- dim(sepal.meas)[1]
p <- length(sepal.meas)
q <- length(petal.meas)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")
##############################################################
##############################################################

# Doing canonical correlation given a sample correlation matrix
# rather than the raw data matrix:

# The Los Angeles Depression Study (n=294 individuals):

# q1 = 2 "health variables":
# CESD:  A numerical measure of depression
# Health: A measure of general perceived health status

# q2 = 4 "personal (~demographic) variables":
# Gender:  Low=Male, High=Female
# Age
# Income
# Education Level

# Suppose the sample correlation matrix R is as given in Table 8.4, page 165:

R22 <- matrix( c(
  1,.044,-.106,-.18,
  .044,1,-.208,-.192,
  -.106,-.208,1,.492,
  -.18,-.192,.492,1 ),
  ncol=4,byrow=T)

R11 <- matrix( c(
  1,.212,.212,1),
  ncol=2,byrow=T)

R12 <- matrix( c(
  .124,-.164,-.101,-.158,
  .098,.308,-.27,-.183),
  ncol=4, byrow=T)

R21 <- t(R12)

# Finding the E1 and E2 matrices:

E1 <- solve(R11) %*% R12 %*% solve(R22) %*% R21
E2 <- solve(R22) %*% R21 %*% solve(R11) %*% R12

# print(E1)
# print(E2)

eigen(E1)
eigen(E2)

# The canonical correlations are:

canon.corr <- sqrt(eigen(E1)$values)
canon.corr

# First canonical variate:

# u1 = 0.46*CESD - 0.89*Health
# v1 = 0.02*Gender + 0.90*Age - 0.41*Education + 0.13*Income

# Second canonical variate:

# u2 = - 0.95*CESD - 0.32*Health
# v2 = - 0.45*Gender + 0.46*Age + 0.47*Education + 0.60*Income

## Bartlett's test for the significance of the first canonical correlation:
## The null hypothesis is that the first (and smaller) canonical correlations are zero.

my.n <- 294; my.q1 <- 2; my.q2 <- 4
test.stat <- -( (my.n-1) - 0.5*(my.q1+my.q2+1) ) * sum(log(1-eigen(E1)$values))
test.stat
P.value <- pchisq(test.stat, df = my.q1*my.q2, lower.tail=F)
P.value

# Since the P-value is tiny, we conclude that there is at least one 
# nonzero canonical correlation.


## Bartlett's test for the significance of the second canonical correlation:
## The null hypothesis is that the second (and smaller) canonical correlations are zero in general, 
## but there's only two here.

my.n <- 294; my.q1 <- 2; my.q2 <- 4
test.stat <- -( (my.n-1) - 0.5*(my.q1+my.q2+1) ) * sum(log(1-eigen(E1)$values[-1]))
test.stat
P.value <- pchisq(test.stat, df = (my.q1-1)*(my.q2-1), lower.tail=F)
P.value

# The P-value is again very small, so we conclude there are at least two 
# nonzero canonical correlations.  In this case, that means exactly two
# nonzero canonical correlations!