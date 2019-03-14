# dimensions for matrix

data <- 1:28

table <- matrix( data, nrow=4, ncol=7, byrow=TRUE, 
          dimnames=list( letters[ 1:4 ], LETTERS[ 1:7 ] ) )

cat( "Matrix...\n" )
print( table )

tier <- table[ 2, ] # return a vector

cat( "\nSubset...\n\nRow #2 (Default)...\n" )
print( tier )

cat( "\nMatrix?:", is.matrix( tier ) )
cat( "\tVector?:", is.vector( tier ), "\n\n" )

tier <- table[ 2, , drop=FALSE ] # return a matrix

cat( "\nRow #2 (Drop=FALSE)...\n" )
print( tier )

cat( "\nMatrix?:", is.matrix( tier ) )
cat( "\tVector?:", is.vector( tier ), "\n" )
