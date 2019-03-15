# portraying stature

frame <- read.csv( "DataSet-Autos.csv" )

library( ggplot2 )

qplot( mpg, wt, data=frame, size=mpg, color=I( "Green" ) )
