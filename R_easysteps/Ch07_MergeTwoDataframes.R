# merge two dataframes

high.temp <- read.csv( "DataSet-HighTemps.csv" )
low.temp <- read.csv( "DataSet-LowTemps.csv" )

display <- function( frame ){
  cat( "\nAnnual Temperatures (°C)...\n" )
  print( frame )
}

display( high.temp )
display( low.temp )

avg.temp <- merge( high.temp, low.temp, 
                   by.x="State", by.y="State.Code" )
display( avg.temp )

avg.temp$Capital <- NULL

avg.temp$Average <- ( avg.temp$High + avg.temp$Low ) / 2
display( avg.temp )
