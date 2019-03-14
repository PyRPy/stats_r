# Transposing data

data <- seq( 1:32 )

table <- matrix( data, nrow=4, ncol=8 )

cat( "\nBy Column (Default):\n\n" )
print( table )

table <- matrix( data, nrow=4, ncol=8, byrow=TRUE )
cat( "\nBy Row:\n\n" )
print( table )

table <- t( table )
cat( "\nTransposed (Rows to Columns):\n\n" )
print( table )
