# 5.9.3 Implementing Satterthwaite's Method -------------------------------

# trout.r, trout experiment, Table 5.13, page 130

trout.data = read.table("trout.txt", header = T)
head(trout.data, 3)

# Read user-defined function from file GamesHowell.r
source("GamesHowell.r")

# Call the function, which returns the results displayed below
GamesHowell(y = trout.data$Hemo, T = trout.data$Sulfa, alpha = 0.05)
