# construct a data.frame

bools <- c( TRUE, FALSE, TRUE )
chars <- LETTERS[ 1:3 ]
nums <- 1:3

frame <- data.frame( bools, chars, nums )
print( frame )
cat( "\nData Frame?:", is.data.frame( frame ), "\n\n" )

rownames( frame ) <- c( "Tier 1:","Tier 2:", "Tier 3:" )
colnames( frame ) <- c( "Logical", "Alphabetical", "Numerical" )

frame[ 2, 2 ] <- "A"
print( frame ) 

cat( "\nSearch for 'A'...\n" )
print( which( frame=="A", arr.ind=TRUE ) )
