# Lesson 6 Stratified Sampling --------------------------------------------
# Chapter 6 TVhour

# On purpose,  the R code here is written in a straightforward way instead 
# of in a sophisticated manner. 

TVhour<-read.delim("TVhour.txt") 
head(TVhour)
#please change the code here to your local address where you save the file
#hint: use "/" if "\" shows in the local address and does not work.

attach(TVhour)
y<-Hour
stratum<-Area
table(stratum)

# Following codes can help you look at mean and variance across stratum
tapply(y,stratum,mean)
tapply(y,stratum,var)

# sample size, mean and SD for strata 1
y1<-y[stratum==1]
n1=length(y1)
y1_bar=mean(y1)
s1=sd(y1)

# sample size, mean and SD for strata 2
y2<-y[stratum==2]
n2=length(y2)
y2_bar=mean(y2)
s2=sd(y2)

# sample size, mean and SD for strata 3
y3<-y[stratum==3]
n3=length(y3)
y3_bar=mean(y3)
s3=sd(y3)

N1=155
N2=62
N3=93
N=N1+N2+N3

# mean estimation
y_bar=(N1*y1_bar+N2*y2_bar+N3*y3_bar)/N
y_bar

# Variance for mean
Var_ybar=((N1/N)^2)*((N1-n1)/N1)*(s1^2)/n1+
          ((N2/N)^2)*((N2-n2)/N2)*(s2^2)/n2+
          ((N3/N)^2)*((N3-n3)/N3)*(s3^2)/n3
Var_ybar

# estimation for total and its variance
tau_hat=N*y_bar
tau_hat
Var_tau=(N^2)*Var_ybar
Var_tau

# dof
a1=N1*(N1-n1)/n1
a2=N2*(N2-n2)/n2
a3=N3*(N3-n3)/n3
d=((a1*(s1^2)+a2*(s2^2)+a3*(s3^2))^2)/(((a1*(s1^2))^2)/(n1-1)+((a2*(s2^2))^2)/(n2-1)+((a3*(s3^2))^2)/(n3-1))
round(d, 1) #because d=21.089, we round it down and set it as 21

dof=21

# 95%CI for mean and total estimation
CI95_ybar=y_bar+(Var_ybar^0.5)*qt(c(.025,.975),dof)
CI95_ybar
CI95_tau=tau_hat+(Var_tau^0.5)*qt(c(.025,.975),dof)
CI95_tau