# ggplot with facets

frame <- read.csv( "DataSet-ProfSalaries.csv" )
library( ggplot2 )

ggplot( data=frame, aes( x=salary )  ) +
  geom_histogram( aes(  fill=rank, color="Black", bins=20 )) +
  facet_grid( rank~. )

ggplot( data=frame, aes( x=salary )  ) +
  geom_histogram( aes( fill=rank ), color="Black", bins=20 ) +
  facet_grid( rank~. , scales="free" )
