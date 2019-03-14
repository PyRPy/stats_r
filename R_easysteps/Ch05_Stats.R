# extracting stattistics 

nums <- rnorm( 20, mean=5, sd=2 )

cat( "Mean:\t", mean( nums ), "\n" )
cat( "Median:\t", median( nums ), "\n\n" )

cat( "Actual SD:\t", sd( nums ), "\n\n" )

cat( "Cut Points:", quantile( nums ), "\n" )

cat( "Total:\t", sum( nums ), "\n\n" )
cat( "Range:\t\t", range( nums ), "\n" )
cat( "Minimum:\t", min( nums ), "\n" )
cat( "Maximum:\t", max( nums ), "\n" )

plot( 1:20, nums, type="o" )
