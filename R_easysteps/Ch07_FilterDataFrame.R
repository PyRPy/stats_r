# filter data frame

frame <- read.csv( "DataSet-Browsers.csv" )

top <- frame[ frame$PerCentage.Market.Share > 10 , ]
cat( "\nTop Browsers...\n" )
print( top )

mid <- frame[ frame$PerCentage.Market.Share > 3 & 
                frame$PerCentage.Market.Share < 10, ]
cat( "\nPopular Browsers...\n" )
print( mid )

google <- frame[ grep( "Chrome", frame$Web.Browser.Version ),  ]
# grep() function is used here to select all rows whose first column cell contains "Chrome" string
cat( "\nGoogle Browsers...\n" )
print( google )
