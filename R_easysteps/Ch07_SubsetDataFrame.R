# subsets for data.frame

frame <- read.csv( "DataSet-Browsers.csv" )

edge <- frame[ c( 33, 24, 5, 12 ),  ]
print( edge )

edge.row <- edge[ 1 , ]

cat( "\nRow...\n")
print( edge.row )

cat( "Data Frame?:", is.data.frame( edge.row ) )

edge.col <- edge[ , 2 ]
cat( "\n\nColumn...\n" )
print( edge.col )

cat( "Data Frame?:", is.data.frame( edge.col ) )
cat( "\tVector?:", is.vector( edge.col ) )

edge.col <- edge[ , 2, drop=FALSE ]
cat( "\n\nColumn...\n" )
print( edge.col )

cat( "Data Frame?:", is.data.frame( edge.col ) )
cat( "\tVector?:", is.vector( edge.col ) )
