# while looping

sum <- 0

i <- 1
while( i < 4 )
{
  sum <- ( sum + 1 )
  cat( "Outer Loop i =", i, "\t\tTotal =", sum, "\n" )
  i <- ( i + 1 )
  
  # Nested loop to be inserted here.
  
  j <- 1
  while( j < 4 )
  {
    sum <- ( sum + 1 )
    cat( "\tInner Loop j =", j, "\tTotal =", sum, "\n" )
    j <- ( j + 1 )
  }
  
}
