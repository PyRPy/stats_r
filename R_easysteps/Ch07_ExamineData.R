# examining data

frame <- read.csv( "DataSet-Browsers.csv" )

cat( "Rows:", nrow( frame ), "\tColumns:", ncol( frame ) )

cat( "\nHead...\n" )
print( head( frame, n=3 ) )

cat( "\nTail...\n" )       
print( tail( frame, n=3 ) )

cat( "\nStructure...\n" )
print( str( frame ) )

cat( "\nSummary...\n" )
print( summary( frame ) )
