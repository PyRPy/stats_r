
# Chapter 11 Response Surface Methods and Designs -------------------------

library(MASS)
library(MontgomeryDAE)
library(FrF2)
library(DoE.base)


# Reaction time and temperature on product yield --------------------------

library(rsm)
head(Table11.1)

df <- Table11.1
# convert natural units to coded
cf <- coded.data(df, x1 ~ (Time- 35) / 5, x2 ~ (Temperature- 155) /5)
model <- rsm(Yield ~ FO(x1, x2), data=cf) # first order, lack of fit, not sig
summary(model)

# add two interaction terms
model<-rsm(Yield~FO(x1,x2)+TWI(x1,x2),data=cf) # lack of fit, not sig
summary(model)

# data table 11.6
head(Table11.6)
cf<-coded.data(Table11.6,x1~(Time-85)/5,x2~(Temperature - 175)/5)
model<-rsm(Yield~FO(x1,x2),data=cf) # lack of fit, significant
summary(model)$lof

# run the second order model
model<-rsm(Yield~SO(x1,x2),data=cf) # lack of fit, not sig
summary(model)$lof

#  response surface plots
contour(model, ~ x1 + x2, image = TRUE)

# D and I optimal design  -------------------------------------------------
library(Vdgraph)
head(Table11.13)
FDSPlot(Table11.13[,c('X1','X2','X3','X4')], mod=2) # D

head(Table11.14)
FDSPlot(Table11.14[,c('X1','X2','X3','X4')], mod=2) # I


# Numerical experimental design with computational model ------------------

# jet engine exahust fume temperature
library(DiceDesign)
#  Latin Hypercube design

df <- lhsDesign(10, 2)$design
colnames(df) <- c('x', 'R')
# Translate the points which are on [0,1] scale to the required scale
df[,'x'] <- df[,'x'] * 0.9 + 0.05
df[,'R'] <- df[,'R'] * 0.062
plot(df, main='LHS Design')

head(Table11.16) # simulation results

# fitting the model
library(DiceEval)
model <- modelFit(Table11.16[,c('x','R')],
                  Table11.16[,'Temperature'],
                  type='Kriging')

# contour plot
x.range<-range(Table11.16[,'x'])
R.range<-range(Table11.16[,'R'])
x<-seq(from=x.range[1],to=x.range[2],length.out=100)
R<-seq(from=R.range[1],to=R.range[2],length.out=100)
z<-matrix(
  modelPredict(model,expand.grid('x'=x, 'R'=R)),
  nrow=length(x)
)
contour(x,R,z,main='GaussianProcessMeanSurface')
