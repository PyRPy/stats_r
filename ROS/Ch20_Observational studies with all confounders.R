
# Observational studies with all confounders assumed to be measured -------
# https://avehtari.github.io/ROS-Examples/Childcare/childcare.html
# not complete; will visit later on
# ChildCare
library("rstan")
library("arm")
library("rstanarm")
library("survey")

# load data
cc2 <- read.csv("ROS_Data/cc2.csv")
head(cc2)

# displays a scatter plot of birthweight versus test scores
par(mfrow=c(1,1))
tmp <- lm(ppvtr.36~bw+treat,data=cc2)$coef
plot(cc2$bw, cc2$ppvtr.36, xlab="birth weight", ylab="test score at age 3", mgp=c(2,.5,0), main="",type="n",xlim=c(1500,5000),cex.axis=.75,cex.lab=.8,lab=c(3,5,7),xaxt="n")
axis(side=1,at=c(2000,3000,4000,5000),cex.axis=.75)
points(cc2$bw[cc2$treat==0]+runif(sum(cc2$treat==0),-.5,5), cc2$ppvtr.36[cc2$treat==0], col="darkgrey", pch=20, cex=.3)
points(cc2$bw[cc2$treat==1]+runif(sum(cc2$treat==1),-.5,5), cc2$ppvtr.36[cc2$treat==1], pch=20, cex=.3)
curve(tmp[1]+tmp[2]*x,add=T,lty=2)
curve(tmp[1]+tmp[3]+tmp[2]*x,add=T)
