title <- "R for Data Analysis"
result <- paste( "Type of title:", typeof( title ) )
print( result )

pi <- 3.14159265
dozen <- 12L

print( paste( "Type of pi:", typeof( pi ) ) )
print( paste( "Type of dozen:", typeof( dozen ) ) )

flag <- T
print( paste( "Is flag logical:", is.logical( flag ) ) )

# do this like :
# source('DataType.R')  # make sure to change it to 'current working directory'