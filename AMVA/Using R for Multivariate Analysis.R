# Using R for Multivariate Analysis ---------------------------------------

# https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html#multivariate-analysis

# Reading Multivariate Analysis Data into R
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep=",")
head(wine)
# write.csv(wine, "wine.csv")

# Plotting Multivariate Data
# A Matrix Scatterplot

# Multivariate Analysis ---------------------------------------------------

library(car)
scatterplotMatrix(wine[2:6])

# A Scatterplot with the Data Points Labelled by their Group
plot(wine$V4, wine$V5)
text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")

# A Profile Plot
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

library(RColorBrewer)
names <- c("V2","V3","V4","V5","V6")
mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
makeProfilePlot(mylist,names)

# Calculating Summary Statistics for Multivariate Data
sapply(wine[2:14],mean)
sapply(wine[2:14],sd)

# Means and Variances Per Group
cultivar2wine <- wine[wine$V1=="2",]
sapply(cultivar2wine[2:14],mean)
sapply(cultivar2wine[2:14], sd)

printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}

printMeanAndSdByGroup(wine[2:14],wine[1])

# Between-groups Variance and Within-groups Variance for a Variable
calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}
calcWithinGroupsVariance(wine[2],wine[1])

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(variable)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the mean and standard deviation for group i:
    meani <- mean(levelidata)
    sdi <- sd(levelidata)
    numi <- levelilength * ((meani - grandmean)^2)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the between-groups variance
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}

calcBetweenGroupsVariance (wine[2], wine[1]) # not working

calcSeparations <- function(variables,groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the separation for each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}

calcSeparations(wine[2:14],wine[1])

# Between-groups Covariance and Within-groups Covariance for Two Variables
calcWithinGroupsCovariance <- function(variable1,variable2,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the covariance of variable 1 and variable 2 for each group:
  Covw <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata1 <- variable1[groupvariable==leveli,]
    levelidata2 <- variable2[groupvariable==leveli,]
    mean1 <- mean(levelidata1)
    mean2 <- mean(levelidata2)
    levelilength <- length(levelidata1)
    # get the covariance for this group:
    term1 <- 0
    for (j in 1:levelilength)
    {
      term1 <- term1 + ((levelidata1[j] - mean1)*(levelidata2[j] - mean2))
    }
    Cov_groupi <- term1 # covariance for this group
    Covw <- Covw + Cov_groupi
  }
  totallength <- nrow(variable1)
  Covw <- Covw / (totallength - numlevels)
  return(Covw)
}
calcWithinGroupsCovariance(wine[8],wine[11],wine[1])

calcBetweenGroupsCovariance <- function(variable1,variable2,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the grand means
  variable1mean <- mean(variable1)
  variable2mean <- mean(variable2)
  # calculate the between-groups covariance
  Covb <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata1 <- variable1[groupvariable==leveli,]
    levelidata2 <- variable2[groupvariable==leveli,]
    mean1 <- mean(levelidata1)
    mean2 <- mean(levelidata2)
    levelilength <- length(levelidata1)
    term1 <- (mean1 - variable1mean)*(mean2 - variable2mean)*(levelilength)
    Covb <- Covb + term1
  }
  Covb <- Covb / (numlevels - 1)
  Covb <- Covb[[1]]
  return(Covb)
}
calcBetweenGroupsCovariance(wine[8],wine[11],wine[1]) # not working

# Calculating Correlations for Multivariate Data
cor.test(wine$V2, wine$V3)

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
mosthighlycorrelated(wine[2:14], 10)

# Standardising Variables
standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
sapply(standardisedconcentrations,mean)
sapply(standardisedconcentrations,sd)


# Principal Component Analysis --------------------------------------------
# standardise the variables
standardisedconcentrations <- as.data.frame(scale(wine[2:14])) 
wine.pca <- prcomp(standardisedconcentrations) 
summary(wine.pca)

wine.pca$sdev

# total variance explained by the components is the sum of the variances 
# of the components
sum((wine.pca$sdev)^2)

# Deciding How Many Principal Components to Retain
# The most obvious change in slope in the scree plot occurs at component 4,
# which is the "elbow" of the scree plot. 
screeplot(wine.pca, type="lines")

# retain principal components for which the variance is above 1
(wine.pca$sdev)^2

# explain at least 80% of the variance, we would retain the first five 
# principal components

# Loadings for the Principal Components
wine.pca$rotation[,1]

# first principal component is a linear combination of the variables
# square of the loadings sum to 1
sum((wine.pca$rotation[,1])^2)

calcpc <- function(variables,loadings)
{
  # find the number of samples in the data set
  as.data.frame(variables)
  numsamples <- nrow(variables)
  # make a vector to store the component
  pc <- numeric(numsamples)
  # find the number of variables
  numvariables <- length(variables)
  # calculate the value of the component for each sample
  for (i in 1:numsamples)
  {
    valuei <- 0
    for (j in 1:numvariables)
    {
      valueij <- variables[i,j]
      loadingj <- loadings[j]
      valuei <- valuei + (valueij * loadingj)
    }
    pc[i] <- valuei
  }
  return(pc)
}
calcpc(standardisedconcentrations, wine.pca$rotation[,1])

# the values of the first principal component are stored in the variable 
# wine.pca$x[,1] 
wine.pca$x[,1]

# an interpretation of the first principal component is that it represents
# a contrast between the concentrations of V8, V7, V13, V10, V12, and V14,
# and the concentrations of V9, V3 and V5.

wine.pca$rotation[,2]

# square of the loadings sum to 1, as above
sum((wine.pca$rotation[,2])^2)

# an interpretation of the second principal component is that it 
# represents a contrast between the concentrations of V11, V2, V14, V4, 
# V6 and V3, and the concentration of V12.

# Scatterplots of the Principal Components
plot(wine.pca$x[,1],wine.pca$x[,2])
text(wine.pca$x[,1],wine.pca$x[,2], wine$V1, cex=0.7, pos=4, col="red") 

# We can see from the scatterplot that wine samples of cultivar 1 have 
# much lower values of the first principal component than wine samples 
# of cultivar 3. Therefore, the first principal component separates wine 
# samples of cultivars 1 from those of cultivar 3.

# the second principal component separates samples of cultivar 2 from 
# samples of cultivars 1 and 3.

# Therefore, the first two principal components are reasonably useful 
# for distinguishing wine samples of the three different cultivars.

