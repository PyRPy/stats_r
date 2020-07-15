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
