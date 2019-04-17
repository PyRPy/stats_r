# exercisebicycle2.r, exercise bicycle experiment, Table 12.15, page 424

bike.data <- read.table("Data/exercise.bicycle.txt", header=T)

# Create factor variables, plus numeric variable TLevel for plotting
bike.data <- within(bike.data,
    {fDay=factor(Day); fSubject=factor(Subject); fDurat=factor(Durat);
     fSpeed=factor(Speed); fPedal=factor(Pedal); fTrtmt=factor(Trtmt);
     # Vbl TLevel with treatment levels 1:8 for equispaced plotting;
     TLevel = 4*(Durat-1) + 2*(Speed-1) + (Pedal-1) })

# ANOVA: treatment combinations
modelTC <- lm(Pulse ~ fDay + fSubject + fTrtmt, data=bike.data)

# Plotting data adjusted for row and column effects
modelTC$coefficients # Display all model coefficient estimates
thetahat <- c(0, modelTC$coefficients[2:8]) # Row effect estimates
thetamean <- mean(thetahat) # Mean row effect estimate
phihat <- c(0, modelTC$coefficients[9:10]) # Column effect estimates
phimean <- mean(phihat) # Mean column effect estimate
# Compute adjusted y-values
bike.data$yadj <- ( bike.data$Pulse 
                      - (thetahat[bike.data$Day] - thetamean)
                      - (phihat[bike.data$Subject] - phimean) )

# Plot adjusted response versus treatment
plot(yadj ~ TLevel, data=bike.data, xaxt="n", xlab="Treatment", 
     ylab="y Adjusted")
axis(1, at=bike.data$TLevel, labels=bike.data$fTrtmt)
