alphabet <- c( "Alpha", "Bravo", "Charlie" )

print( alphabet )

print( paste( "2nd Element: ", alphabet[2] ) )

print( paste( "Vector Length: ", length( alphabet ) ) )

alphabet[ 5 ] <- "Echo"

print( alphabet )
print( paste( "Vector Length Now: ", length( alphabet ) ) )

print( paste( "Is alphabet a Vector: ", is.vector( alphabet ) ) )

# to try without 'paste()' function :
print( "Is alphabet a Vector: ", is.vector( alphabet ))

# only print out "Is alphabet a Vector: "