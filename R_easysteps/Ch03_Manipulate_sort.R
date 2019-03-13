# Manuipulating elements

fruit <- c( "Banana", "Apple", "Cherry" )

cat( "Fruit:", fruit , "\n" )

fruit <- sort( fruit )

cat( "Sorted:", fruit , "\n\n" )

nums <-  c( NA, 8:2, NA, 1:7, NA  )

cat( "Numbers:", nums , "\n" )

nums <- sort( nums, na.last=TRUE ) # without na.last=TRUE, NA removed automatically

cat( "Increasing:", nums , "\n" )

nums <- sort( nums , decreasing=TRUE )
cat( "Decreasing:", nums , "\n" )

nums <- rev( nums )
cat( "Reversed:", nums , "\n" )

nums <- unique( nums )
cat( "Unique:", nums , "\n" )
