# Chapter 12 Clustering ---------------------------------------------------

# Example 12.3 Clustering using single linkage ----------------------------

# package stats is already installed
library("stats")
# need to enter distances using dist
Dist=as.dist(matrix(c(0,9,3,6,11,9,0,7,5,10,
                      3,7,0,9,2,6,5,9,0,8,
                      11,10,2,8,0),5,5))
hcS=hclust(Dist,method="single")
plot(hcS) 


# Example 12.4 Single linkage clustering of 11 languages ------------------

# Need to install package cluster
library("cluster") # default in base R now
# enter similarities and convert to  distances using dist
Sim =as.matrix(read.table("Data/E12-4.dat",header=F),11,11)

m10=matrix(c(rep(10,121)),11,11)
Dist=as.dist(m10-Sim)
clangS=agnes(Dist,method="single") # Agglomerative Nesting 
pltree(clangS,labels=c("E","N","Da","Du","G","Fr","Sp","I","P","H","Fi") ) 


# Example 12.5 Clustering using complete linkage --------------------------

library("stats")
# need to enter distance matrix using dist
Dist=as.dist(matrix(c(0,9,3,6,11,9,0,7,5,10,
                      3,7,0,9,2,6,5,9,0,8,
                      11,10,2,8,0),5,5))
hcC=hclust(Dist,method="complete")
plot(hcC) # column number is 'vector'


# Example 12.6 Complete linkage clustering of 11 languages ----------------

library("stats")
#enter similarities and convert to  distances using dist
Sim =as.matrix(read.table("Data/E12-4.dat"),11,11)
m10=matrix(c(rep(10,121)),11,11)
Dist=as.dist(m10-Sim)
hclangS=hclust(Dist,method="complete")
plot(hclangS,labels=c("E","N","Da","Du","G","Fr","Sp","I","P","H","Fi") )
## Du and G combine at height 5 . Text incorrect


# Example 12.7 Clustering variables using complete linkage ----------------

dat=read.table("Data/T12-4.dat")
head(dat)
Rcor= cor(dat[,1:8]) # V9 is company name

# similarities need to used---not distances
## Use correlations between variables "as distance"
dd <- as.dist((1 - Rcor)/2)
(hcC= hclust(dd,method="complete")  )
plot(hcC)
# maximum height is ( 1 - r )/2 so 1.561/2=.78


# Example 12.8 average linkage clustering of 11 languages -----------------

library("stats")
# enter similarities and convert to  distances using dist
Sim =as.matrix(read.table("Data/E12-4.dat"),11,11)
m10=matrix(c(rep(10,121)),11,11)
Dist=as.dist(m10-Sim)
hclangS=hclust(Dist,method="average")
plot(hclangS,labels=c("E","N","Da","Du","G","Fr","Sp","I","P","H","Fi") )


# Example 12.9 Average linkage - public utilities -------------------------

library("stats")
#read in triangular matrix as vector
x=c(0.00,3.10, 0.00,3.68, 4.92, 0.00, 2.46, 2.16, 4.11, 0.00, 4.12, 3.85, 4.47, 4.13, 0.00, 3.61, 4.22, 2.99, 3.20, 4.60, 0.00,  +
      3.90, 3.45, 4.22, 3.97, 4.60, 3.35, 0.00,  +
      2.74, 3.89, 4.99, 3.69, 5.16, 4.91, 4.36, 0.00, +
      3.25, 3.96, 2.75, 3.75, 4.49, 3.73, 2.80, 3.59, 0.00, +
      3.10, 2.71, 3.93, 1.49, 4.05, 3.83, 4.51, 3.67, 3.57, 0.00,  +
      3.49, 4.79, 5.90, 4.86, 6.46, 6.00, 6.00, 3.46, 5.18, 5.08, 0.00,  +
      3.22, 2.43, 4.03, 3.50, 3.60, 3.74, 1.66, 4.06, 2.74, 3.94, 5.21, 0.00,  +
      3.96, 3.43, 4.39, 2.58, 4.76, 4.55, 5.01, 4.14, 3.66, 1.41, 5.31, 4.50, 0.00,  +
      2.11, 4.32, 2.74, 3.23, 4.82, 3.47, 4.91, 4.34, 3.82, 3.61, 4.32, 4.34, 4.39, 0.00,  +
      2.59, 2.50, 5.16, 3.19, 4.26, 4.07, 2.93, 3.85, 4.11, 4.26, 4.74, 2.33, 5.10, 4.24, 0.00,  +
      4.03, 4.84, 5.26, 4.97, 5.82, 5.84, 5.04, 2.20, 3.63, 4.53, 3.43, 4.62, 4.41, 5.17, 5.18, 0.00, +
      4.40, 3.62, 6.36, 4.89, 5.63, 6.10, 4.58, 5.43, 4.90, 5.48, 4.75, 3.50, 5.61, 5.56, 3.40, 5.56, 0.00, +
      1.88, 2.90, 2.72, 2.65, 4.34, 2.85, 2.95, 3.24, 2.43, 3.07, 3.95, 2.45, 3.78, 2.30, 3.00, 3.97, 4.43, 0.00,  +
      2.41, 4.63, 3.18, 3.46, 5.13, 2.58, 4.52, 4.11, 4.11, 4.13, 4.52, 4.41, 5.01, 1.88, 4.03, 5.23, 6.09, 2.47, 0.00, +
      3.17, 3.00, 3.73, 1.82, 4.39, 2.91, 3.54, 4.09, 2.95, 2.05, 5.35, 3.43, 2.23, 3.74, 3.78, 4.82, 4.87, 2.92, 3.90, 0.00, +
      3.45, 2.32, 5.09, 3.88, 3.64, 4.63, 2.68, 3.98, 3.74, 4.36, 4.88, 1.38, 4.94, 4.93, 2.10, 4.57, 3.10, 3.19, 4.97, 4.15, 0.00, +
      2.51, 2.42, 4.11, 2.58, 3.77, 4.03, 4.00, 3.24, 3.21, 2.56, 3.44, 3.00, 2.74, 3.51, 3.35, 3.46, 3.63, 2.55, 3.97, 2.62, 3.01, 0.00)

# convert lower triangular  to symmetric matrix
X=matrix(rep(0,484),22,22)
X[upper.tri(X,diag=T)]<-x
Dat=X+t(X)
Dist =as.dist(Dat)
hclangS=hclust(Dist,method="average")
plot(hclangS )


# Example 12.11 Clustering using the k-means method -----------------------

x1=c(5,-1,1,-3)
x2=c(3,1,-2,-2)
dat=cbind(x1,x2)
rownames(dat)=c("A","B","C","D")
(kmc=kmeans(dat,2))

# checking the squared distances
t(dat[1,]-kmc$centers[1,])%*% (dat[1,]-kmc$centers[1,])
t(dat[2,]-kmc$centers[1,])%*% (dat[2,]-kmc$centers[1,])
t(dat[3,]-kmc$centers[1,])%*% (dat[3,]-kmc$centers[1,])
t(dat[4,]-kmc$centers[1,])%*% (dat[4,]-kmc$centers[1,])

t(dat[1,]-kmc$centers[2,])%*% (dat[1,]-kmc$centers[2,])
t(dat[2,]-kmc$centers[2,])%*% (dat[2,]-kmc$centers[2,])
t(dat[3,]-kmc$centers[2,])%*% (dat[3,]-kmc$centers[2,])
t(dat[4,]-kmc$centers[2,])%*% (dat[4,]-kmc$centers[2,])


# Example 12.12 K-means clustering of public utilities --------------------

dat=read.table("Data/T12-4.dat")

# should standardize data with scale command
(kmc4=kmeans(scale(dat[,1:8]),4))
table(dat[,9],kmc4$cluster)

# distances between centers
dist(kmc4$centers)
(kmc5=kmeans(scale(dat[,1:8]),5))
table(dat[,9],kmc5$cluster)
dist(kmc5$centers)
# A little different from text. 

# Example 12.13 Model based clustering of iris data -----------------------

#need to install package mclust
library(mclust)
datiris=read.table("Data/T11-5.dat")
head(datiris)

xone=rep(1,150)
z2=.5*xone%*%t(c(1,1))
resVVV2=me(modelName = "VVV",z=z2,G=2, data = datiris[,-5],x=NULL)
resVVV2$parameters
z3=xone%*%t(c(1,1,1))/3
resVVV3=me(modelName = "VVV",z=z3,G=3, data = datiris[,-5],x=NULL)
resVVV3$parameters

# search several models with three components
resul3=Mclust(data=datiris[,-5], G = 2, modelNames = NULL,x =  NULL)
resul3$BIC              #BIC picks model VEV
resul3$parameters
# search several models with two components
resul2=Mclust(data=datiris[,-5], G = 2, modelNames = NULL,x =  NULL)
#BIC picks model VEV with 2 components as slighty better than 3 component
resul2$BIC    
resul2$parameters 

