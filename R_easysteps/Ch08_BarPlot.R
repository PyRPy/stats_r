# bar plot for rolling a dice

nums <- sample( 1:6, 30, replace=TRUE )

print( nums )

library( ggplot2 )

qplot( nums, geom="bar",
       color=I("Blue"),
       fill=I("Lightblue"),
       xlab="Face",
       ylab="Frequency",
       main="30 Shakes of the Dice"
) + scale_x_continuous( breaks = 1:6, 
    labels=c("One", "Two", "Three", "Four", "Five", "Six" ) )
