# smooth lines

frame <- read.csv( "DataSet-Autos.csv" )

print( tail( frame ) )

library( ggplot2 )

qplot( mpg, wt, data=frame, geom=c( "point", "line" ) )

qplot( mpg, wt, data=frame, geom=c( "point", "smooth" ) )
