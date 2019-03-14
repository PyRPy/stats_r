# locate data

frame <- read.csv( "DataSet-Browsers.csv" )

cat( "Head...\n" )
print( head( frame, n=3 ) )

data <- frame[ 1, 2 ]
cat( "\nRow #1, Column #2:", data , "\n" )

data <- frame[ 2, "PerCentage.Market.Share" ]
cat( "\nRow #2, Column #2:", data, "\n" )

data <- frame$PerCentage.Market.Share[ 3 ]
cat( "\nRow #3, Column #2:", data, "\n\n" )

print( levels( frame$Web.Browser.Version ) )
