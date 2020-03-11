# ANOVA for cluster variance analysis -------------------------------------
# ref: https://online.stat.psu.edu/stat506/node/41/
# to construct a data table
number <- c(3,5,6,4,5,6,3,2,4,5,
            2,0,2,1,1,0,1,1,0,1,
            3,2,3,2,4,2,2,1,2,2,
            5,2,3,2,1,1,2,2,4,1)
cluster <- c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10))
numberOfPhones <- data.frame(cluster = cluster,
                             number = number)
# check the table
numberOfPhones

# convert to factor
numberOfPhones$cluster <- as.factor(numberOfPhones$cluster)

# visualize the cluster effect
boxplot(number ~ cluster, data = numberOfPhones,
        ylab = "Number of cell phones per household",
        main = "Cluster sampling from households")

# stats summary
summary(numberOfPhones)

# run a linear model
mod_lm <- lm(number ~ cluster, data = numberOfPhones)

# oneway anaova
anova(mod_lm)

# Response: number
# Df Sum Sq Mean Sq F value    Pr(>F)    
# cluster    3   58.7  19.567  16.306 7.395e-07 ***
# Residuals 36   43.2   1.200 
