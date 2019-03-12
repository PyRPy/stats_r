# Make comparisons
nil <- 0
num <- 0
max <- 1

cap <- "A"
low <- "a"

print( paste( "0 == 0 Equality:", nil == num ) )
print( paste( "A == a Equality:", cap == low ) )

print( paste( "0 != 1 Inequality:", nil != max ) )

print( paste( "0 > 1 Greater:", nil > max ) )
print( paste( "0 < 1 Less:", nil < max ) )

print( paste( "0 >= 0 Greater or Equal:", nil >= num ) )
print( paste( "1 <= 0 Less or Equal:", max <= nil ) )