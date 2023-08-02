
# 9.3 Randomized experiments ----------------------------------------------

## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company

library ("arm")
electric <- read.table ("ARM_Data/electric.company/electric.dat", header=T)
head(electric)
attach(electric)

## Plot of the raw data (Figure 9.4)

onlytext <- function(string){
  plot(0:1, 0:1, bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
  text(0.5, 0.5, string, cex=1.2, font=2)
}
nf <- layout (matrix(c(0,1:14), 5, 3, byrow=TRUE), c(5, 10, 10),
              c(1, 5, 5, 5, 5), TRUE)
par (mar=c(.2,.2,.2,.2))
onlytext ('Test scores in control classes')
onlytext ('Test scores in treated classes')

par (mar=c(1,1,1,1), lwd=0.7)
for (j in 1:4){
  onlytext(paste ('Grade', j))
  hist (control.Posttest[Grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n',
        main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text (2, 6.5, paste ("mean =", round (mean(control.Posttest[Grade==j]))), adj=0)
  text (2, 5, paste ("  sd =", round (sd(control.Posttest[Grade==j]))), adj=0)

  hist (treated.Posttest[Grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n',
        main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text (2, 6.5, paste ("mean =", round (mean(treated.Posttest[Grade==j]))), adj=0)
  text (2, 5, paste ("  sd =", round (sd(treated.Posttest[Grade==j]))), adj=0)
}

## Basic analysis of a completely randomized experiment

post.test <- c (treated.Posttest, control.Posttest)
pre.test <- c (treated.Pretest, control.Pretest)
grade <- rep (Grade, 2)
treatment <- rep (c(1,0), rep(length(treated.Posttest),2))
n <- length (post.test)

for (k in 1:4){
  display (lm (post.test ~ treatment, subset=(grade==k)))
}


# 9.4 Treatment interactions and poststratification -----------------------

## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company

# The R codes & data files should be saved in the same directory for
# the source command to work

# source("9.3_Randomized experiments.R") # where data was cleaned

## Treatment interactions and poststratification

# model with only treat. indicator

lm.1 <- lm (post.test ~ treatment, subset=(grade==4))
display (lm.1)

# model controlling for pre-test

lm.2 <- lm (post.test ~ treatment + pre.test, subset=(grade==4))
display (lm.2)


## model to display uncertainty & Figure 9.8

lm.4 <- lm (post.test ~ treatment + pre.test + treatment:pre.test, subset=(grade==4))
display (lm.4)

# compute the average treatment effect & summarize
lm.4.sim <- sim (lm.4)
n.sims <- nrow(coef(lm.4.sim))
effect <- array (NA, c(n.sims, sum(grade==4)))
for (i in 1:n.sims){
  effect[i,] <- coef(lm.4.sim)[i,2] + coef(lm.4.sim)[i,4]*pre.test[grade==4]
}
avg.effect <- rowMeans (effect)

print (c (mean(avg.effect), sd(avg.effect)))

# 9.5 Observational studies -----------------------------------------------

# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company

# The R codes & data files should be saved in the same directory for
# the source command to work

# Plot Figure 9.9

supp <- c(ifelse(electric[,"Supplement."] == "S", 1, 0), rep(NA,nrow(electric)))
# supp=0 for replace, 1 for supplement, NA for control
# supp <- ifelse(electric[,"Supplement."] == "S", 1, 0)
est1 <- rep(NA,4)
se1 <- rep(NA,4)
ok <- (grade==1) & (!is.na(supp))

lm.supp <- lm (post.test ~ supp + pre.test, subset = ok)


for (k in 1:4){
  ok <- (grade==k) & (!is.na(supp))
  lm.supp <- lm (post.test ~ supp + pre.test, subset=ok)
  est1[k] <- coef(lm.supp)[2]
  se1[k] <- summary(lm.supp)$coef[2,2]
}
# finally fixed dimension problem. but not fininsed!
