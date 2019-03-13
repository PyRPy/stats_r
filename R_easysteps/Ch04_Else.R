# Branching alternatives 

hour <- 21

if( hour < 13 )
{
  print( paste( "Good Morning:", hour ) )
} else
  {
    if( hour < 18 ) print( paste( "Good Afternoon:", hour ) )
    else
    {
      print( paste( "Good Evening:", hour ) )
    }
  }
