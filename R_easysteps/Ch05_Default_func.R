# default for functions input

launch <- function( num=5 )
{
  cat( num, "- " )
  num <- ( num - 1 )
  
  if( num < 0 ) { return( NULL ) } else { launch( num ) }
}

graph <- function( x, y, ... ) # three dots allows the lable text to be passed tot he plot() func
{
  plot( x, y, col="Red", type="o", ... )
}

launch( )
graph( 1:20, rnorm( 20 ), xlab="X Axis", ylab="Y Axis" )
