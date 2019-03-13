# producing sequences :

half.year <- month.abb[ 1:6 ]
cat( "Constant:", half.year, "\n" )

cat( "Sequence:", seq( 1, 8 ), "\n" )
cat( "Two Step:", seq( 1, 8, 2 ), "\n\n" )

cat( "Distributed:", seq( 1, 8, length.out=4 ), "\n" )
cat( "Distributed:", seq( 1, 8, along.with = half.year ), "\n\n" )

cat( "Replicated:", rep( 5, 9 ), "\n" )

cat( "Replicated:", rep( 1:4, 3 ), "\n" )

cat( "Replicated:", rep( 1:4, 3, each=2 ), "\n" )

# build-in constants
LETTERS

letters

month.name

month.abb

letters[1:3]
