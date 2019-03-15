# ggplot 

frame <- read.csv( "DataSet-ProfSalaries.csv" )
library( ggplot2 )

ggplot( data=frame, aes( x=rank ) ) +
  geom_bar( fill="Yellow", color="Red" )

ggplot( data=frame, aes( x=salary ) ) + 
  geom_histogram( fill="Purple", color="White", bins=20 )

ggplot( data=frame, aes( x=salary ) ) +
  geom_histogram(  aes( fill=rank), color="Black", bins=20 )
