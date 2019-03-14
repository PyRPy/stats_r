# factors in data

frame <- data.frame( 1:5, sizes=c( "S", "L", "XL", "S", "M") )

cat( "\nColumn Data...\n" )
str( frame$sizes )

var.sizes <- c( "S", "L", "XL", "S", "M" )

cat( "\nVector Data...\n" )
str( var.sizes )

var.sizes <- factor( var.sizes )

cat( "\nFactored Vector Data...\n" )
str( var.sizes )
print( levels( var.sizes ) )

var.sizes <- factor( var.sizes, 
                     levels = c( "S","M","L","XL" ) )

cat( "\nRe-ordered Factored Vector Data...\n" )
str( var.sizes )
print( levels( var.sizes ) )

var.sizes <- factor( var.sizes, 
                     levels=rev( levels( var.sizes ) ) )
cat( "\nReversed Factored Vector Data...\n" )
str( var.sizes )
print( levels( var.sizes ) )
