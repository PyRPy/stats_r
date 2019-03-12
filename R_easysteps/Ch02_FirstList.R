sales <- list( Jan=1500, Feb=1300, Mar=2400 )

sales <- c( sales, list( Apr=1800 ) )

print( unlist( sales ) )

# try print(sales) directly; it prints different format without 'unlist()'
print(sales)

monthly.sales <- unlist( sales, use.names=FALSE )

total.sales <- sum( monthly.sales )
print( paste( "Total Sales: ", total.sales ) )

average.per.month <- mean( monthly.sales )
print(paste( "Monthly Average: ", average.per.month ) )

print( paste( "Is sales a List: ", is.list( sales ) ) )
