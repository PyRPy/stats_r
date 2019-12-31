# Chapter 7 Introduction to data management using data.table

# set the formatting parameters -------------------------------------------

library(data.table)
options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20,
        datatable.print.topn = 3,
        digits = 2)

iris$Species <- as.character(iris$Species)
diris <- as.data.table(iris)
diris

tables()

sapply(diris, class)

haskey(diris) # from data.table, no key
setkey(diris, Species)
key(diris)

haskey(diris) # has key

diris <- diris[order(Sepal.Length)] # order
diris
haskey(diris)

diris <- diris[order(Sepal.Length, -Sepal.Width)]
diris

setkey(diris, Species)
diris[49:52]

anyDuplicated(diris)
anyDuplicated(diris$Sepal.Length)

unique(diris)


# selecting and subsetting data -------------------------------------------

diris[1:5]
diris[-(1:148)]
diris["setosa"]
diris[!"setosa"]
diris[Sepal.Length < 5]
diris[Sepal.Length < 5 & Petal.Width < 0.2]
diris[Sepal.Length == 4.3 | Sepal.Length == 4.4]

interest <- c(4.3, 4.4)
diris[Sepal.Length %in% interest]

diris[!Sepal.Length %in% interest]

diris[, Sepal.Length] # use second formal ?
is.data.table(diris[, Sepal.Length])
diris[, .(Sepal.Length)] # return a data.table

diris[, .(Sepal.Length, Sepal.Width)]

x <- "example"
x
"x"
diris[, 1, with=FALSE]
diris[, "Sepal.Length", with=FALSE]

v <- "Sepal.Length"
diris[, v, with=FALSE]
diris[,v] # wrong ?
diris[, 1]

diris[1, -"Sepal.Length", with=FALSE]

diris[1, !"Sepal.Length", with=FALSE]

diris[1, -c("Sepal.Length", "Petal.Width"), with=FALSE]

diris[1, -v, with=FALSE]

head(diris[[v]]) # return a vector
head(diris[, Sepal.Length])
head(diris$Sepal.Length)


# vairable renaming and ordering ------------------------------------------

names(diris)
colnames(diris)
setnames(diris, old="Sepal.Length", new="SepalLength")
names(diris)

setnames(diris, old = 1, new = "SepalL")
names(diris)

setcolorder(diris, c("SepalL", "Petal.Length", "Sepal.Width", 
                     "Petal.Width", "Species"))
diris

v <- c(1, 3, 2, 4, 5)
setcolorder(diris, v)
diris


# computing on data and creating variables --------------------------------

diris <- as.data.table(iris)
setkey(diris, Species)

diris[, V0:=0]
diris[, Sepal.Length:=NULL]
diris[, c("X1", "X2"):=.(1L, 2L)]
diris[, V:=Petal.Length * Petal.Width]
diris

diris[, c("V", "V0"):=NULL]
diris[1]

diris["setosa", V:=Petal.Length * Petal.Width]
unique(diris)

mean(diris$Sepal.Width)
diris[, mean(Sepal.Width)]

diris[, .(M=mean(Sepal.Width))]

diris[, .(M=mean(Sepal.Width)), by=Species]

diris[, .(M1=mean(Sepal.Width), M2=mean(Petal.Width)), 
                  by=Species][, .(r=cor(M1, M2))]


# merging and reshaping data ----------------------------------------------

# merge data
d2key1 <- data.table(ID1 = c(1, 1, 2, 2), ID2 = c(1, 2, 1, 2),
                     X = letters[1:4])
d2key2 <- data.table(ID1 = c(1, 1, 2, 2), ID2 = c(1, 2, 1, 2),
                     Y = LETTERS[1:4])

d2key1
d2key2

setkey(d2key1, ID1)
setkey(d2key2, ID1)

merge(d2key1, d2key2)

# reshaping data
diris <- as.data.table(iris)
diris[, ID:=1:.N]
setkey(diris, ID)
diris

diris.long <- melt(diris, measure.vars = 
                list(c("Sepal.Length", "Sepal.Width"), c("Petal.Length", 
                                                         "Petal.Width")),
                variable.name = "Type", value.name = c("Sepal", "Petal"),
                id.vars = c("ID", "Species"))
diris.long

diris.long2 <- melt(diris, id.vars = c("ID", "Species"))
diris.long2

diris.wide <- dcast(diris.long, ID + Species ~ Type, 
                    value.var = list("Sepal", "Petal"),
                    sep = ".")
diris.wide
diris.wide2 <- dcast(diris.long2, ID + Species ~ variable)
diris.wide2

all.equal(diris.wide, diris.wide2) # different column names
