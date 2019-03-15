# groups in the plot

frame <- read.csv( "DataSet-Autos.csv" )

library( ggplot2 )

Cylinders <- factor( frame$cyl )

qplot( mpg, wt, data=frame, size=I( 5 ), shape=Cylinders, color=Cylinders )

