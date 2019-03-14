# creating a matrix

data <- seq( 1:32 )

table <- matrix( data, nrow=4, ncol=8 ) # column by column when filling numbers

print( table )

cat( "\nVector?:", is.vector( table ), 
     "\tMatrix?:", is.matrix( table ) )

cat( "\n\nCell 4,5 Contains:", table[ 4, 5 ] )
table[ 4, 5 ] <- 10

cell <- which( table==10, arr.ind=TRUE )
cat( "\n\nValue 10 Search:\n" )
print( cell )