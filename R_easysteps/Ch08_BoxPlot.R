# boxplot

frame <- read.csv( "DataSet-Experiment.csv" )

print( head( frame, n=3 ) )
str(frame)

library( ggplot2 )

qplot( Group, Weight, data=frame, 
       geom=c( "boxplot", "jitter" ), 
       fill=Group,
       main="Experiment Results" 
)

qplot( Group, Weight, data=frame, 
       geom="violin", trim=FALSE, 
       fill=Group,
       main="Experiment Results" 
)
