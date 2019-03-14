# generating random numbers :

rand <- runif( 1 )
cat( "Random Number:\t\t", rand, "\n" )

rand <- ( rand * 10 )
cat( "Multiplied Number:\t", rand, "\n" )

rand <- ceiling( rand )
cat( "Random Integer:\t\t", rand, "\n\n" )

for (i in seq(1:10) )
{
  print(paste("Number of runs= ", i ))
  print(paste("Random number= ", round(runif(1), 2)))
  
}

random_curve <- rnorm(100)
plot(random_curve)
