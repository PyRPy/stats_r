# Comparing elements

ascend <- c( 1:5 )
descend <- c( 5:1 )

cat( "Vectors:\n", ascend,"\n", descend )

result <- ascend > descend

cat( "\n1st Vector Greater?:", result )

cat( "\nAt Index No.:", which( result ) )

pets <- c( "Dog", "Cat", "Gerbil", "Rabbit" )
animals <- c( "Lion","Tiger", "Cat", "Rabbit" )

cat( "\n\nVectors:\n", pets,"\n",animals )

result <- pets == animals

cat( "\nElement Match?:", result ) # element-wise comparison/match

cat( "\nAt Index No.:", which( result ) )

cat( "\nCommon:", intersect( pets, animals ) )
