# for loops by index, slow speed, be aware

seq <- list( A="Alpha", B="Bravo", C="Charlie" )

for( var in seq )
{
  print( var )
}

seq <- c( 2, 7, 6, 8, 3, 5, 4 )

for( var in seq )
{
  if( var %% 2 == 1 )
  {
    cat( var, "Is Odd\n")
  } else
  {
    cat( var, "Is Even\n")
  }
}
