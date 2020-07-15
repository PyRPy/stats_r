# Chapter 08 Principal Components

# Example 8.1 Calculate the population PC ---------------------------------
# find eigen values and eigen vectors
SIG=matrix(c(1,-2,0,-2,5,0,0,0,2),3,3)
eig8.1=eigen(SIG)
eig8.1$values
eig8.1$vectors

# sum(eig8.1$values * eig8.1$vector)
# eig8.1$values[1] * eig8.1$vector[, 1] + 
#   eig8.1$values[2] * eig8.1$vector[, 2] + 
#   eig8.1$values[3] * eig8.1$vector[, 3]

# to conform with text example, multiply the first eigen vector by -1
eig8.1$vector[,1]=-eig8.1$vector[,1]
rhoY1X1=eig8.1$vector[1,1]*sqrt(eig8.1$value[1])/sqrt(SIG[1,1])
rhoY1X1 # 0.9238795

rhoY1X2=eig8.1$vector[2,1]*sqrt(eig8.1$value[1])/sqrt(SIG[2,2])
rhoY1X3=eig8.1$vector[3,1]*sqrt(eig8.1$value[1])/sqrt(SIG[3,3]) 
rhoY2X1=eig8.1$vector[1,2]*sqrt(eig8.1$value[2])/sqrt(SIG[1,1])
rhoY2X2=eig8.1$vector[2,2]*sqrt(eig8.1$value[2])/sqrt(SIG[2,2])
rhoY2X3=eig8.1$vector[3,2]*sqrt(eig8.1$value[2])/sqrt(SIG[3,3]) 


# Example 8.2 PC obtained from convariance and correlation M are d --------

SIG=matrix(c(1,4,4,100),2,2)
eig8.2=eigen(SIG)
eig8.2$values
eig8.2$vectors

# to conform with text example, multiply the second eigen vector by -1
eig8.2$vector[,2]=-eig8.2$vector[,2]
corY1X1= eig8.2$vector[1,1]*sqrt( eig8.2$value[1])/sqrt(SIG[1,1])
corY1X2=eig8.2$vector[2,1]*sqrt( eig8.2$value[1])/sqrt(SIG[2,2])
corY2X1=eig8.2$vec[1,2]*sqrt( eig8.2$value[2])/sqrt(SIG[1,1])
corY2X2=eig8.2$vector[2,2]*sqrt( eig8.2$value[2])/sqrt(SIG[2,2])

# propotion explained
eig8.2$values[1]/sum(eig8.2$values)

RCOR=cov2cor(SIG)
eigR8.2=eigen(RCOR)
eigR8.2$value
eigR8.2$vector

# to conform with text example, multiply the second eigen vector by -
RcorY1Z1= eigR8.2$vector[1,1]*sqrt( eigR8.2$value[1])
RcorY1Z2=eigR8.2$vector[2,1]*sqrt( eigR8.2$value[1])
RcorY2Z1=eigR8.2$vec[1,2]*sqrt( eigR8.2$value[2])
RcorY2Z2=eigR8.2$vector[2,2]*sqrt( eigR8.2$value[2])

# proportion explained
eigR8.2$values[1]/ncol(RCOR)


# Example 8.3 Sample variability iwth two sample PCs ----------------------

cen=read.table("Data/T8-5.dat")
pc= eigen(cov(cen))
pc
# Note the first three eigenvectors are the negatives of those in the text 
# so we calculate the correlations of first two  principal components with 
# variables by inserting a minus sign
cor(-as.matrix(cen)%*%pc$vectors[,1:2],cen)

# scree plot 
plot(seq(1:5),pc$values,type="l",col="blue",bty="n",
     xlab="number",
     ylab=expression(hat(lambda))) 

# you could also use the command princomp but the eigenvalues are a 
# little different. 
pc.cen=princomp(cen)
pc.cen$loadings
plot(pc.cen,type="lines",col="blue")

# Example 8.4 Summarize sample var with one sample PC ---------------------

turtle=read.table("Data/T6-9.dat")
head(turtle)
names(turtle) <- c("len", "wid", "ht")
# take logs and put in file turt
turt=transform(turtle, lnlen=log(len),lnwid=log(wid),lnht=log(ht))

names(turt)=c("len","wid","ht","gender","lnlen","lnwid","lnht")
head(turt)

pc.lnturt=princomp(turt[25:48,5:7])
loadings(pc.lnturt)

# the first principal component is the negative of that in text
# To find the correlations of first principal component with the first variable
cor(-pc.lnturt$score[,1],turt[25:48,5:7]) 

# the scree plot
plot(pc.lnturt,type="lines",col="blue")


# Example 8.5 Sample PC from standardized data ----------------------------
library(quantmod)
library(PerformanceAnalytics)
# Stock watch list --------------------------------------------------------

# a collection of stocks
mystocks <- c("MSFT", "ZM", "PYPL", "DHR", "TMO")

# get data
getSymbols(mystocks, from = "2020-01-01", src = "yahoo")

wkret1 <- weeklyReturn(MSFT)
wkret2 <- weeklyReturn(ZM)
wkret3 <- weeklyReturn(PYPL)
wkret4 <- weeklyReturn(DHR)
wkret5 <- weeklyReturn(TMO)

df_ret <- data.frame(msft = wkret1, zm = wkret2, pypl=wkret3,
                     dhr = wkret4, tmo = wkret5)
head(df_ret)
names(df_ret) = c("msft", "zm", "pypl", "dhr", "tmo")
# get weekly return average
weeklyret <- c(mean(weeklyReturn(MSFT)),
             mean(weeklyReturn(ZM)),
             mean(weeklyReturn(PYPL)),
             mean(weeklyReturn(DHR)),
             mean(weeklyReturn(TMO)))
weeklyret

# corr matrix
round(cor(df_ret), 2)

# eigen values / vectors
pc_ret <- eigen(cor(df_ret))
pc_ret$values
pc_ret$vectors
sum(pc_ret$values) # equal to 5 
# first two PC account for :
sum(pc_ret$values[1:2]) / sum(pc_ret$values) # 83.09%

# use princomp
pc.ret=princomp(cor(df_ret))
pc.ret$loadings
plot(pc.ret,type="lines",col="blue")


# Example 8.6 Components fr a corr matrix with special struct -------------

R=matrix(c(1,.7501,.6329,.6363,.7501,1,.6925,.7386,
           .6329,.6925,1,.6625,.6363,.7386,.6625,1), 4,4)
eigen(R)
# The first principal component( eigen vector) is the negative of the one 
# in the text


# Example 8.7 Plot the principal components for turtle data ---------------

turtle=read.table("Data/T6-9.dat")

# take logs and put in columns 5 to 7 of file turt
turt=transform(turtle, lnlen=log(turtle[,1]),lnwid=log(turtle[,2]),lnht=log(turtle[,3]))
names(turt)=c("len","wid","ht","gender","lnlen","lnwid","lnht")

# we use princomp  to obtain the scores
pc.lnturt=princomp(turt[25:48,5:7])

# normal scores for the normal scores plot
xns=qnorm(1:24/25)
plot(xns,sort(pc.lnturt$scores[,2]),pch=19,xlab="normal scores",ylab="pc2")

# pc.lnturt$loadings shows that the first principal component is the negative 
# of the one in the text we introduce a minus sign for the second plot
plot(pc.lnturt$scores[,2],-pc.lnturt$scores[,1],
     xlab="pc2",ylab="pc1",pch=19,col="blue")
