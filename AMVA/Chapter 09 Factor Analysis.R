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


# Example 9.3 Factor analysis of consumer preference data -----------------

# You need to install the package psych
library(psych)

# Enter the correlation  matrix
Rho=matrix(c(1,.02,.96,.42,.01,.02,1,.13,.71,.85,
             .96,.13,1,.5,.11,.42,.71,.5,1,.79,
             .01,.85,.11,.79,1),5,5)

fit =principal(Rho, nfactors=2,rotate="none")
fit$loadings  #loadings, communalities, and specific variances
L=round(fit$loadings[,1:2],2)   # to closely match decimals in example
round(diag(L%*%t(L)),2)  # are the communalities
Psi=round(diag(1-L%*%t(L)),2)  #are the specific variances
Psi


# Example 9.4 Factor analysis of stock-price data -------------------------

# install package psych and use the command principal
library(psych)
stock=read.table("Data/T8-4.DAT")
head(stock)

names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")

# to view the coefficients of the principal components as extracted 
# from cor  matrix
fit=principal(stock,nfactors=2,residuals=TRUE,rotate="none")
fit
fit$loadings  #loadings, communalities, and specific variances 
residuals(fit) 
# residuals for off diagonal elements.Rather than 0's the diagonal 
# elements are specific variances


# Example 9.5 Factor analysis of stock-price using maximum likelih --------

library(psych)
# use command fa for mle. Principal components from E9-4r.
stock=read.table("Data/T8-4.DAT")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
fit=fa(stock,2,rotate="none",fm="ml",residuals=TRUE)
fit                #slight differences from text
fit$loadings  #loadings, communalities, and specific variances
residuals(fit)  
# residuals but with 0 replaced by specific variances on diagonals

fitpc=principal(stock,nfactors=2,residuals=TRUE,rotate="none")
fitpc
fitpc$loadings  #loadings, communalities, and specific variances
residuals(fitpc)


# Example 9.6 Olympic decathlon data --------------------------------------

# principal component solution use command principal from library psych
# use command fa for mle
library(psych)
Rdec=read.table("Data/E9-6.dat")
rownames(Rdec)=c("100m","long mp","shotput","highjmp", "400m","hurtles",
                 "discuss","polevlt","javelin","1500mrun")
eig=eigen(Rdec)   # shows that 3 or 4 factors are reasonable
eig # 4.2129434 1.3895855 1.0594001 0.9177767

fitpc=principal(Rdec,nfactors=4,rotate="none",residuals=TRUE)
fitpc$loadings  #loadings, communalities, and specific variances
residuals(fitpc)     #specific variances on diagonal not 0


fitml=fa(Rdec,4,residuals=TRUE,fm="ml",rotate="none")
fitml$loadings  #loadings, proportions

residuals(fitml)


# Example 9.7 Testing for two common factors ------------------------------

# install package psych and use the command principal
library(psych)
stock=read.table("Data/T8-4.DAT")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
n=103 # n = nrow(stock)
R=cor(stock)

fit=fa(stock,2,fm="ml",rotate="none")
fit$loadings  #loadings, communalities, and specific variances
L=fit$loadings
diag(diag(1-L%*%t(L)))+L%*%t(L)   #matrix approximation by factor model
m=ncol(L)
p=ncol(R)
(n-1-(2*p+4*m+5)/6)*log(det(diag(diag(1-L%*%t(L)))+L%*%t(L) )/det(R))
# compare with
qchisq(.95,(((p-m)^2-p-m)/2)) # 2.0 < 3.84, failed to reject Ho


# Example 9.8 first look at factor rotation -------------------------------

library(psych)
R=read.table("Data/E9-8.dat")
names(R)=c("Gaelic","English","History","Arithmetic","Algebra","Geometry")
# to view the coefficients of the ml solution as extracted from cor  matrix
fit=fa(R,2,rotate="none",fm="ml",residuals=TRUE)
fit$loadings  #loadings, communalities, and specific variances
residuals(fit)     #specific variances on diagonals

# residuals and specific variances different from text
# plot loadings
plot(fit$loadings,bty="n",
     xlab=expression(F[1]),
     ylab=expression(F[2]),
     pch=19)
text(fit$loadings[,1],fit$loadings[,2]+.022,c("1","2","3","4","5","6"))

# the rotation
alp=2*pi*20/360   # the 20 degree angle in radians
T=matrix(c(cos(alp),-sin(alp),sin(alp),cos(alp)),2,2)
# rotated loadings
fit$loadings[,1:2]%*%T  


# Example 9.9 Rotated loadings for cosumer-preference data ----------------

library(psych)
# Enter the correlation  matrix
Rho=matrix(c(1,.02,.96,.42,.01,.02,1,.13,.71,.85,
             .96,.13,1,.5,.11,.42,.71,.5,1,.79,
             .01,.85,.11,.79,1),5,5)

fit <- principal(Rho, nfactors=2,rotate="varimax",residuals=TRUE)
fit$loadings  #loadings, communalities, and specific variances
residuals(fit)      # specific variances on diagonal

# plot of rotated factors
plot(fit,xlab=expression(F[1]),ylab=expression(F[2]))    


# Example 9.10 Rotated loadings for stock-price data ----------------------

library(psych)
stock=read.table("Data/T8-4.dat")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
fit=fa(stock,2,rotate="none",fm="ml",residuals=TRUE)
fit$loadings  #loadings, communalities, and specific variances
residuals(fit)   #comunalities on diagonal

# with varimax rotation
fitr=fa(stock,2,rotate="varimax",fm="ml",residuals=TRUE)
fitr$loadings  #loadings, communalities, and specific variances
# loadings slightly  different from text


# Example 9.11 Rotated loadings for Olymptic decathlon data ---------------

# principal component solution use command principal from library psych
# use command fa for mle
library(psych)
Rdec=read.table("Data/E9-6.dat")   #enter correlation matrix
rownames(Rdec)=c("100m","long mp","shotput","highjmp", "400m",
                 "hurtles","discuss","polevlt","javelin","1500mrun")

fitpc=principal(Rdec,nfactors=4,rotate="varimax",residuals=TRUE)
fitpc$loadings  #loadings, communalities, and specific variances
residuals(fitpc)

fitml=fa(Rdec,4,rotate="varimax",fm="ml",residuals=TRUE)
fitml$loadings  #loadings, communalities, and specific variances
residuals(fitml)

# plots
plot(fitml$loadings[,1],fitml$loadings[,2],xlab="Factor 1",ylab="Factor 2",pch=19)
text(fitml$loadings[,1]+.02,fitml$loadings[,2]+.01,seq(1:10))

plot(fitml$loadings[,1],fitml$loadings[,3],xlab="Factor 1",ylab="Factor 3",pch=19)
text(fitml$loadings[,1]+.02,fitml$loadings[,3]+.01,seq(1:10))


# Example 9.12 Computing factor scores ------------------------------------

# use command fa form psych library for mle
library(psych)
stock=read.table("Data/T8-4.dat")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
R=cor(stock)
fitmlr=fa(stock,2,rotate="varimax",fm="ml")
fitmlr$loadings  #loadings, communalities, and specific variances

# the argument at which to evaluate factor scores
z=c(.5,-1.4,-.2,-.7,1.4) 
fsregr=factor.scores(R,fitmlr,method=c("Thurstone")) #regression scores
t(matrix(fsregr$weights, 5, 2))%*%matrix(z, 5, 1) # fix inconsistencies ?

fsBartr=factor.scores(R,fitmlr,method=c("Bartlett"))
#both sets of factor scores change a little when more digits
t(fsBartr$weights)%*%z

# are carried in the calculation plot
# plot of factor scores
# create regression factor scores
fsreg=as.matrix(stock,103,5)%*%fsregr$weights        
plot(fsreg[,1],fsreg[,2],pch=19,
                        xlab="Scores for Factor 1",
                        ylab="Scores for Factor 2")


# Example 9.14 Factor analysis of chicken-bone data -----------------------

library(psych)
dat=read.table("Data/E9-14.dat")
names(dat)=c("skullen","skullbr","femurlen","tibialen","humerlen","ulnalen")
R=cor(dat)
round(R, 3)

fitmlr=fa(dat,3,fm="ml",rotate="varimax",residual=TRUE)
fsrml=factor.scores(dat,fitmlr,method=c("Thurstone"))
plot(fsrml$scores[,1],fsrml$scores[,2],
     xlab="First factor scores--mle",
     ylab="Second factor scores--mle")

cat("\n **** method = principal **** \n")
faprinr=principal(dat,3,rotate="varimax")
faprinr

fsprinr=factor.scores(dat,faprinr,method=c("Thurstone"))
plot(fsrml$scores[,1],fsprinr$scores[,1],
     xlab="First factor scores--mle",
     ylab="First factor scores--Principal component")

plot(fsrml$scores[,2],fsprinr$scores[,2],xlab="Second factor scores--mle",
     ylab="Second factor scores--Principal component")

plot(fsrml$scores[,3],fsprinr$scores[,3],xlab="Third factor scores--mle",
     ylab="Third factor scores--Principal component")

# split the data---mle  is difficult to obtain. This is the Heywood case and 
# apparently the solution is not well determined. 
# As we suggest in the text, 3 factors is too many  and the best approach is 
# to use a 2 factor solution

dat1=dat[1:137,]
dat2=dat[138:276,]
cor(dat1)
cor(dat2)
fitmlr1d=fa(dat1,3,fm="ml",rotate="varimax") #different from text. 
fitmlr2d=fa(dat2,3,fm="ml",rotate="varimax") #different from text 
# not good, not aligned with  y = 1 * x line