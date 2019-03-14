# binding vectors

start <- LETTERS[ 1:10 ]
finish <- LETTERS[ 17:26 ]
numeric <- seq( 1:10 )

table <- rbind( start, finish, numeric )
cat( "\nBind Rows:\n\n")
print( table )

table <- cbind( start, finish, numeric )
cat( "\nBind Columns:\n\n")
print( table )
