# P.166 - Loading the data
frame <- read.csv( "DataSet-Hurricanes.csv" )

head( frame, n=8 )

library( ggplot2 )

ggplot( data=frame, aes( x=Year, y=DamageMillions, 
                         size=DamageMillions,
                         color=WindMPH ) ) + geom_point( )
#-----------------------------------------------------------
# P.168 - Retaining objects
damage_plot <- ggplot( data=frame,
                  aes( x=Year, y=DamageMillions,
                  size=DamageMillions,
                  color=WindMPH ) )

damage_plot + geom_point( )

damage_plot + geom_point( ) + geom_line( size=0.5 )
#-----------------------------------------------------------
# P.170 - Overriding labels
damage_plot + ggtitle( "US Atlantic Hurricanes" ) +
xlab( "Event Year" ) + ylab( "Damage $ Millions" ) +
geom_point( ) +
geom_line( size=0.5 )

label_object <- labs( title="US Atlantic Hurricanes", 
                      subtitle="1950-2012",
                      x="Event Year",
                      y="Damage $ Millions",
                      caption="Source: Wikipedia" )
damage_plot + 
label_object +
geom_point( ) +
geom_line( size=0.5 ) 
#----------------------------------------------------------
# P.172 - Adding a theme - NOT WORKING
windowsFonts()

# install.packages( "extrafont", dependencies=TRUE )

library( extrafont )

font_import( prompt=FALSE )

loadfonts( device = "win" )

length( windowsFonts( ) )

theme_object <- theme(
  plot.title=element_text( color="Red", family="Wide Latin" ),
  plot.subtitle=element_text( color="Red" ),
  axis.title.x=element_text( color="Red", face="bold" ),
  axis.title.y=element_text( color="Red", face="bold" ),
  plot.caption=element_text( color="Black", face="italic" ),
  legend.background = element_rect( color="Gray" ) )

damage_plot +
label_object +
geom_point( ) +
geom_line( size=0.5 ) +
theme_object
#----------------------------------------------------------
#P.175 - Comparing boxes (Boxplot)
head( frame, n=8 )

gender_plot <- ggplot( data=frame,
                       aes( x=Sex, y=Deaths, color=Sex ), 
                       size=3 )

label_object <- labs( title="US Atlantic Hurricanes", 
                      subtitle="1950-2012",
                      x="Event Year",
                      y="Damage $ Millions",
                      caption="Source: Wikipedia" )

theme_object <- theme(
  plot.title=element_text( color="Red", family="Wide Latin" ),
  plot.subtitle=element_text( color="Red" ),
  axis.title.x=element_text( color="Red", face="bold" ),
  axis.title.y=element_text( color="Red", face="bold" ),
  plot.caption=element_text( color="Black", face="italic" ),
  legend.background = element_rect( color="Gray" ) )


gender_plot + 
label_object + 
  xlab( "Gender" ) + 
  ylab( "Number of Fatalities" ) +
geom_jitter( ) +
geom_boxplot( alpha=0.5 ) + ylim( 10, 200 ) +
theme_object

#----------------------------------------------------------
# P.176 - Identifying extremes
fatal_plot <- ggplot( data=frame, 
                      aes( x=Year, y=Deaths ) )
fatal_plot +
label_object +
ylab( "Number of Fatalities" ) +
geom_point( aes( color=WindMPH )) + 
geom_text( aes( label=ifelse( Deaths > 180,
                as.character( Name ), "" ),
                hjust=1.1 ) 
            ) +
geom_smooth() +
theme_object
#-----------------------------------------------------------

# P.178 - Limiting focus
windspeed_plot <- ggplot( data=frame, 
                      aes( x=Year, y=Deaths ) )
windspeed_plot +
geom_point( aes( color=WindMPH ), size=10, shape=17 ) + 
label_object +
ylab( "Number of Fatalities" ) +
xlim( 1980, 2000 ) +
ylim( 0, 65 ) +
theme_object

#-----------------------------------------------------------
# P.179 - Zooming focus
strike_plot <- ggplot( data=frame, aes( x=Year, fill=Sex ) )

strike_plot +
label_object +
ylab( "Number of Strikes" ) +
geom_bar( ) +
coord_cartesian( xlim=c( 1980, 2000 ) ) +
theme_object

#----------------------------------------------------------- 
# P.180 - Displaying facets
frame.tx <- frame[ grep( "TX", frame$AffectedStates ) , ]

texas_plot <- ggplot( data=frame.tx, 
                      aes( x=Year,y=DamageMillions,
                          size=Deaths, color=AffectedStates))
texas_plot + 
label_object + 
geom_point( aes( size=Deaths ) ) + 
facet_grid( AffectedStates~.  ) + 
theme_object

#-----------------------------------------------------------
# P.181 - cont'd
frame.fl <- frame[ grep( "FL", frame$AffectedStates ) , ]

florida_plot <- ggplot( data=frame.fl, aes(x=Year ) )

# not very pleasant, sometimes
florida_plot +
label_object +
ylab( "Number of Strikes in Florida" ) +
geom_histogram( bins=20, aes(fill=AffectedStates),
                  color="Black" ) +
theme_object
#--End------------------------------------------------------