# Using R for Multivariate Analysis ---------------------------------------

# https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html#multivariate-analysis

# Reading Multivariate Analysis Data into R
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep=",")
head(wine)
table(wine$V1)
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
  grandmean <- mean(as.numeric(unlist(variable))) # fixed problem ? so complicated
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


# Linear Discriminant Analysis --------------------------------------------

library(MASS)
wine.lda <- lda(wine$V1 ~ wine$V2 + wine$V3 + wine$V4 + wine$V5 + wine$V6 + 
                  wine$V7 + wine$V8 + wine$V9 + wine$V10 + wine$V11 + 
                  wine$V12 + wine$V13 + wine$V14)

# Loadings for the Discriminant Functions
wine.lda
wine.lda$scaling[,1]
calclda <- function(variables,loadings)
{
  # find the number of samples in the data set
  as.data.frame(variables)
  numsamples <- nrow(variables)
  # make a vector to store the discriminant function
  ld <- numeric(numsamples)
  # find the number of variables
  numvariables <- length(variables)
  # calculate the value of the discriminant function for each sample
  for (i in 1:numsamples)
  {
    valuei <- 0
    for (j in 1:numvariables)
    {
      valueij <- variables[i,j]
      loadingj <- loadings[j]
      valuei <- valuei + (valueij * loadingj)
    }
    ld[i] <- valuei
  }
  # standardise the discriminant function so that its mean value is 0:
  ld <- as.data.frame(scale(ld, center=TRUE, scale=FALSE))
  ld <- ld[[1]]
  return(ld)
}

calclda(wine[2:14], wine.lda$scaling[,1])
# the values of the first linear discriminant function can be calculated 
# using the “predict()” function in R

wine.lda.values <- predict(wine.lda, wine[2:14])
wine.lda.values$x[,1]

groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(as.numeric(unlist(variablei))) # fixed inconsistency ?
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[`variablei_name`] <- variablei_new
  }
  return(variables_new)
}

groupstandardisedconcentrations <- groupStandardise(wine[2:14], wine[1])

wine.lda2 <- lda(wine$V1 ~ groupstandardisedconcentrations$V2 + groupstandardisedconcentrations$V3 +
                   groupstandardisedconcentrations$V4 + groupstandardisedconcentrations$V5 +
                   groupstandardisedconcentrations$V6 + groupstandardisedconcentrations$V7 +
                   groupstandardisedconcentrations$V8 + groupstandardisedconcentrations$V9 +
                   groupstandardisedconcentrations$V10 + groupstandardisedconcentrations$V11 +
                   groupstandardisedconcentrations$V12 + groupstandardisedconcentrations$V13 +
                   groupstandardisedconcentrations$V14)

# The loadings for V8, V13 and V14 are negative, while those for V11 and V5 
# are positive. Therefore, the discriminant function seems to represent a c
# ontrast between the concentrations of V8, V13 and V14, and the concentrations 
# of V11 and V5.

# the values of the discriminant function are the same regardless of whether we standardise the input 
# variables or not. 
wine.lda.values <- predict(wine.lda, wine[2:14])
wine.lda.values$x[,1]

wine.lda.values2 <- predict(wine.lda2, groupstandardisedconcentrations)
wine.lda.values2$x[,1]

# Separation Achieved by the Discriminant Functions
wine.lda.values <- predict(wine.lda, standardisedconcentrations)
calcSeparations(wine.lda.values$x,wine[1])

# The “proportion of trace” that is printed when you type “wine.lda” 
# (the variable returned by the lda() function) is the percentage separation 
# achieved by each discriminant function.
wine.lda

(wine.lda$svd)^2

# A Stacked Histogram of the LDA Values
# seperate 1 and 3
ldahist(data = wine.lda.values$x[, 1], g = wine$V1)
# seperate 2 from 1 and 3
ldahist(data = wine.lda.values$x[,2], g=wine$V1)

# Scatterplots of the Discriminant Functions
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) 
text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$V1,cex=0.7,pos=4,col="red")

# Allocation Rules and Misclassification Rate
printMeanAndSdByGroup(wine.lda.values$x,wine[1])

calcAllocationRuleAccuracy <- function(ldavalue, groupvariable, cutoffpoints)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the number of true positives and false negatives for each group
  numlevels <- length(levels)
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- ldavalue[groupvariable==leveli]
    # see how many of the samples from this group are classified in each group
    for (j in 1:numlevels)
    {
      levelj <- levels[j]
      if (j == 1)
      {
        cutoff1 <- cutoffpoints[1]
        cutoff2 <- "NA"
        results <- summary(levelidata <= cutoff1)
      }
      else if (j == numlevels)
      {
        cutoff1 <- cutoffpoints[(numlevels-1)]
        cutoff2 <- "NA"
        results <- summary(levelidata > cutoff1)
      }
      else
      {
        cutoff1 <- cutoffpoints[(j-1)]
        cutoff2 <- cutoffpoints[(j)]
        results <- summary(levelidata > cutoff1 & levelidata <= cutoff2)
      }
      trues <- results["TRUE"]
      trues <- trues[[1]]
      print(paste("Number of samples of group",leveli,"classified as group",levelj," : ",
                  trues,"(cutoffs:",cutoff1,",",cutoff2,")"))
    }
  }
}

calcAllocationRuleAccuracy(wine.lda.values$x[,1], wine[1], c(-1.751107, 2.122505))
# There are 3+5+1=9 wine samples that are misclassified, 
# out of (56+3+5+65+1+48=) 178 wine samples

