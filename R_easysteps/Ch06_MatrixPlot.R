# plot matrix

ny <- c( 3.8, 5.5, 9.9, 15.7, 21.5, 26.3 )
la <- c( 19.5, 19.4, 19.7, 20.8, 21.3, 22.7 )
fw <- c( 13.7, 15.4, 20, 24.6, 28.5, 32.7 )

table <- cbind( ny, la, fw )
print( table )

matplot( table, type="b", pch=15:17, col=2:4 )
