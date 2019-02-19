# exercisebicycle.r, exercise bicycle experiment, Table 12.12 p420, 
# and code from text p423

# Table 12.12, p420
bike.data <- read.table("Data/exercise.bicycle.txt", header=T)
head(bike.data, 3)

# Create factor variables
bike.data <- within(bike.data, 
  {fDay=factor(Day); fSubject=factor(Subject); fDurat=factor(Durat);
   fSpeed=factor(Speed); fPedal=factor(Pedal); fTrtmt=factor(Trtmt) })

# ANOVA: treatment combinations
modelTC <- lm(Pulse ~ fDay + fSubject + fTrtmt, data=bike.data)
anova(modelTC)
drop1(modelTC, ~., test = "F") 

# Treatment contrasts
library(lsmeans)
lsmTrtmt <- lsmeans(modelTC, ~ fTrtmt) 
lsmTrtmt
summary(contrast(lsmTrtmt, 
        list(DurationDiff=c(-1,-1,-1,-1, 1, 1, 1, 1)/4,
                SpeedDiff=c(-1,-1, 1, 1,-1,-1, 1, 1)/4,
                PedalDiff=c(-1, 1,-1, 1,-1, 1,-1, 1)/4)), 
        infer=c(T,T))

# Compute variables for residual plots
bike.data <- within(bike.data, {
   # Compute predicted, residual, and standardized residual values
   ypred = fitted(modelTC)
   e = resid(modelTC)
   z = e/sd(e) 
   # Compute Blom's normal scores
   n = length(e)
   q = rank(e)
   nscore = qnorm((q-0.375)/(n+0.25))
   # Compute variable TLevel with treatment levels 1:8 for equispaced plotting
   TLevel = 4*(Durat-1) + 2*(Speed-1) + (Pedal-1) })

# Residual plots
plot(z ~ TLevel, data=bike.data, xaxt="n", xlab="Treatment")
  axis(1, at=bike.data$TLevel, labels=bike.data$fTrtmt)
plot(z ~ ypred + Day + Subject + nscore, data=bike.data)

### Hit return key for each plot above, before entering the code below!

# Code from text p423
# input data again (refresh inputs)
# Table 12.12, p420
bike.data <- read.table("Data/exercise.bicycle.txt", header=T)
head(bike.data, 3)

# Create factor variables
bike.data <- within(bike.data, 
                    {fDay=factor(Day); fSubject=factor(Subject); fDurat=factor(Durat);
                    fSpeed=factor(Speed); fPedal=factor(Pedal); fTrtmt=factor(Trtmt) })

# ANOVA: factorial effects
modelFE <- lm(Pulse ~ fDay + fSubject + Durat*Speed*Pedal, 
              data=bike.data)

# modelFE <- lm(Pulse ~ fDay + fSubject + fDurat*fSpeed*fPedal, 
#              data=bike.data)

summary(modelFE)
anova(modelFE)
drop1(modelFE, ~., test = "F") 
# need to be fixed (this section)
# Treatment contrasts
lsmDurat <- lsmeans(modelFE, ~ Durat) 
summary(contrast(lsmDurat, list(DurationDiff=c(-1, 1))), infer=c(T,T))

lsmSpeed <- lsmeans(modelFE, ~ Speed) 
summary(contrast(lsmSpeed, list(SpeedDiff=c(-1, 1))), infer=c(T,T))

lsmPedal <- lsmeans(modelFE, ~ Pedal) 
summary(contrast(lsmPedal, list(PedalDiff=c(-1, 1))), infer=c(T,T))
