# select columns in data.frame

frame <- read.csv( "DataSet-Browsers.csv" )

display <- function( title ) { 
  cat( "\n", title, "...\n" )
  print( head( frame, n=2 ) )
}

display("Original Columns" )

colnames( frame ) <- c( "Web.Browser", "PerCentage" )
display( "Renamed Columns" )

frame$Market.Share <- 
  paste( as.character( frame$PerCentage ),"%", sep="" )
frame$PerCentage <-  NULL
display( "Switched Columns" )
