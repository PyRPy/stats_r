# select themes

frame <- read.csv( "DataSet-ProfSalaries.csv" )
library( ggplot2 )

ggplot( data=frame, aes( x=rank, y=salary ) ) +
  geom_jitter( aes( color=rank ) ) +
  geom_boxplot( aes( color=rank ), alpha=0.5 ) +
  
  theme( 
    
    axis.title.x=element_text( size=15, color="Red" ),
    axis.title.y=element_text( size=15, color="Blue" ),
    legend.title=element_text( size=15 ),
    axis.text.x=element_text( size=15, color="Red" ),
    axis.text.y=element_text( size=15, color="Blue" ),
    legend.text=element_text( size=15 ),
    
    legend.justification=c( 0, 1 ),
    legend.position=c( 0.02, 0.97 )

    )
