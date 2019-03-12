# to define list and data frame first
qtr.1 <- list( Jan=1500, Feb=1300, Mar=2400 )
qtr.2 <- list( Apr=1800, May=1700, Jun=2800 )
qtr.3 <- list( Jul=3100, Aug=3800, Sep=3200 )
qtr.4 <- list( Oct=2600, Nov=2200, Dec=2400 )
qtr.1

year <- unlist( c( qtr.1, qtr.2, qtr.3, qtr.4 ) )
year
str(year)

# it includes plot, axises, and box...
plot( year, type="o", col="Blue", pch=15,
      ann=FALSE, axes=FALSE )
axis( 1, at=1:12, lab=c( names( year ) ) )
axis( 2 )
title( xlab="Month", ylab="$",
       main="Yearly Sales", col.main=rgb( 1, 0, 0 ) )
box( )
