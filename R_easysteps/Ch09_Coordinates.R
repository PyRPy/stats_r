# coordinates in plots

frame <- read.csv( "DataSet-ProfSalaries.csv" )
library( ggplot2 )
 
ggplot( data=frame, aes( x=salary ) ) +
  geom_histogram( aes( fill=rank), color="Black", bins=20 ) +
  xlim( 80000, 140000 ) +
  ylim( 0, 40 )

ggplot( data=frame, aes( x=salary ) ) +
  geom_histogram(  aes( fill=rank), color="Black", bins=20 ) +
  coord_cartesian( xlim=c( 80000, 140000 ), ylim=c( 0, 40 ) )
