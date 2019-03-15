# start using ggplot

frame <- read.csv( "DataSet-ProfSalaries.csv" )

head( frame, n=10 )
str(frame)

library( ggplot2 )

ggplot( data=frame, aes( x=yrs.since.phd, y=salary ) ) +
  geom_point( )
