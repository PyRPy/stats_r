# ggplot with stats

frame <- read.csv( "DataSet-ProfSalaries.csv" )
library( ggplot2 )

ggplot( data=frame, aes( x=salary ) ) +
  geom_density( aes( fill=rank ) )

ggplot( data=frame, aes( x=yrs.since.phd, y=salary ) ) +
  geom_point( aes( color=rank ) ) +
  geom_smooth( aes( color=rank ), fill=NA )

ggplot( data=frame, aes( x=rank, y=salary ) ) +
  geom_jitter( aes( color=rank) ) +
  geom_boxplot(  aes( fill=rank ), alpha=0.5 ) 
