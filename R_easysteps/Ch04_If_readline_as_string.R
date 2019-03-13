# Seeking truth

if ( 5 > 1 )
{
  print( "Five is greater than one." )
}

if ( 2 < 4 )
{
  print( "Two is less than four." )
  print( "Test succeeded." )
}

num <- 8

if( ( num > 5 ) && ( num < 10 ) || ( num == 12 ) )
{
  print( "Number is 6-9 inclusive, or 12" )
}

# input from console
num <- as.numeric(readline("please type a number: " )) # input as 'string' not a number !!!

if( ( num > 5 ) && ( num < 10 ) || ( num == 12 ) )
{
  print( "Number is 6-9 inclusive, or 12" )
}
