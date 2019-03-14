# Distributions :

qty <- 10

while( qty <= 1000000 )
{
  num <- 0
  for( i in rnorm( qty, mean=0, sd=1 ) )
  {
    if( ( i >= -1 ) && ( i <= 1 ) ) num <- ( num + 1 )
  }
  
  num <-  ( num / ( qty / 100 ) )
  cat( "For", qty, "Generated Random Numbers:", num,"%\n" )
  
  qty = ( qty * 10 )
}
