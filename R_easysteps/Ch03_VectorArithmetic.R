# Operating on elements
series <- c( 1:9 )
cat( "Series:", series, "\n" ) 

slice <- series[ 1:3 ]
cat( "Slice:", slice, "\n" )

cat( "Slice:", slice )

totals <- series + slice # will add in recycling fashion
cat( "Totals:", totals, "\n" )

slice <- series[ 1:4 ]
cat( "New Slice:", slice, "\n" )

totals <- series + slice # # will add in recycling fashion
cat( "New Totals:", totals, "\n" )
