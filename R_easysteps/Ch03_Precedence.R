# Recognizing precedence :

sum <- 1 + 4 * 3

print( paste( "Default Order:", sum ) )

sum <- ( 1 + 4 ) * 3

print( paste( "Forced Order:", sum ) )

sum <- 7 - 4 + 2

print( paste( "Default Direction:", sum ) )

sum <- 7 - ( 4 + 2 )

print( paste( "Forced Direction:", sum ) )
