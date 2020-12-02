# startup.r, Section 3.9 , page 58
rm(list = ls())    # Remove all objects (start with clean slate) 
opar = par()       # Save default graphics parameters as opar
setwd("/RCode")    # Set the working directory
getwd()            # Get/display name of working directory
options(show.signif.stars=FALSE) # Show no stars for significance tests
options(width=72, digits=5, scipen=2) # Control printed output 
options = options(width=72, digits=5, scipen=2) # Save print options