
# First step EDA ----------------------------------------------------------
# read data - gold coins weight

coin_weight <- read.table("Ex6_2-4.txt", quote="\"", comment.char="")
names(coin_weight) <- "weight"

# five number summary
summary(coin_weight$weight)

boxplot(coin_weight$weight,
        horizontal = TRUE,
        main = "Box plot for weights of 39 gold coins")

