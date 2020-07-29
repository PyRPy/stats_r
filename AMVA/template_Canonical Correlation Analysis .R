# Canonical Correlation Analysis  -----------------------------------------
# ref https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/

# load packages
require(ggplot2)
require(GGally)
require(CCA)
require(CCP)

# read data
# The psychological variables are locus_of_control, self_concept and 
# motivation. The academic variables are standardized tests in 
# reading (read), writing (write), math (math) and science (science). 
# Additionally, the variable female is a zero-one indicator variable with 
# the one indicating a female student.

mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
# write.csv(mm, "mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)

# Canonical correlation analysis
xtabs(~Sex, data = mm)

psych <- mm[,1:3]
acad <- mm[, 4:8]

ggpairs(psych)
ggpairs(acad)

# look at the correlations within and between the two sets of variables 
# using the matcor function 
matcor(psych, acad)

# R Canonical Correlation Analysis
cc1 <- cc(psych, acad)
cc1$cor # display the canonical correlations

cc1[3:4] # raw canonical coefficients

# compute canonical loadings
cc2 <- comput(psych, acad, cc1)

# display canonical loadings
cc2[3:6]

# tests of canonical dimensions
rho <- cc1$cor

# Define number of observations, number of variables in first set, and 
# number of variables in the second set.
n <- dim(psych)[1]
p <- length(psych)
q <- length(acad)

# Calculate p-values using the F-approximations of different test 
# statistics:
p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")

# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% cc1$ycoef

## Cautions, Flies in the Ointment
# 
# - Multivatiate normal distribution assumptions are required for both sets 
#   of variables.
# - Canonical correlation analysis is not recommended for small samples.