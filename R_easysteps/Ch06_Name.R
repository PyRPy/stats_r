# names for rows and columns

ny <- c( 3.8, 5.5, 9.9, 15.7, 21.5, 26.3 )
la <- c( 19.5, 19.4, 19.7, 20.8, 21.3, 22.7 )
fw <- c( 13.7, 15.4, 20.0, 24.6, 28.5, 32.7)

table <- rbind( ny, la, fw )
print( table )

rownames( table ) <- c( "New York", 
                        "Los Angeles", 
                        "Fort Worth" )

colnames( table ) <- month.abb[ 1:6 ]

cat( "\nAverage High Temperature (°C):\n\n" )
print( table )

nyc <- table[ "New York", ]  # Or table[ 1, ]

cat( "\nNew York:", nyc, "\n\n" )

print( nyc )
