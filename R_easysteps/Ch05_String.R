# work with strings

string <- "R for Data Analysis"

cat( "Substring:\t", substr( string, 7, 10 ), "\n" )

cat( "Replaced:\t", sub( "sis", "tics", string ), "\n\n" )

print( paste( "Split: ", strsplit( string, " " ) ) )

cat( "\nUppercase:\t", toupper( string ), "\n\n" )

now <-  Sys.time( )
print( format( now, format="Date: %A, %B %e" ) )
print( format( now, format="Time: %H:%M" ) )
