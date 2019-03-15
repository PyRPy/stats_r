# histograms

Sex <- rep( c( "Female", "Male" ), each=500 )

height <- c( rnorm( 500, 65 ), rnorm( 500, 69 ) )

frame <- data.frame( Sex, height )

head( frame, n=3 )
tail( frame, n=3 )

library( ggplot2 )

qplot( height, data=frame, geom="histogram", 
       fill=Sex, bins=40,
       ylab="Frequency", xlab="Height in Inches",
       main="Average Adult Height (USA)" )

qplot( height, data=frame, geom="density", 
       size=I(2), color=Sex, linetype=Sex,
       ylab="Density", xlab="Height in Inches",
       main="Average Adult Height (USA)" )
