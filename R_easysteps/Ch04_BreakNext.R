# breading from loops : break, next
for( i in 1:3 )
{
  for( j in 1:3 )
  {
    if( i==1 && j==1)
    {
      cat( "Skips Iteration at i=", i, " and j=", j, "\n" )
      next
    }
    
    if( i==2 && j==1)
    {
      cat( "Breaks Inner Loop at i=", i, " and j=", j, "\n" )
      break
    }
    
    cat( "Running i=", i," j=", j, "\n" )
  }
}
