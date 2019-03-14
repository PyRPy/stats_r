# creating functions

greet <- function( )
{ 
  print( "Hello from R!" ) 
}

greet( )

f2c <- function( degrees )
{
  result <- ( ( degrees - 32 ) * ( 5 / 9 ) )
  return( result )
}

cat( "Body Temperature 98.6 °F =", f2c(98.6), "°C\n\n")

print( result ) # result as a variable is not a 'globle' variable
