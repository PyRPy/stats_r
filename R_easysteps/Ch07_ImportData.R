# import data sets

setwd( "C:/MyRScripts")
cat( "Working Directory:", getwd( ), "\n\n" )

frame <- read.csv( "DataSet-Browsers.csv" )

print( frame )

frame <- read.csv(file.choose())
View(frame) # use View not view !!!
