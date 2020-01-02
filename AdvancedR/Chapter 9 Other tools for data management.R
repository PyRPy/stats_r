# Chapter 9 Other tools for data management
library(checkpoint)
library(dplyr)
library(tibble)
options(stringsAsFactors = FALSE,
        tibble.print_max = 20,
        tibble.print_min = 5,
        digits = 2)

iris$Species <- as.character(iris$Species)
diris <- as.tibble(iris)
diris

# sorting
diris <- arrange(diris, Sepal.Length)
diris

diris <- arrange(diris, Sepal.Length, desc(Sepal.Width))
diris

anyDuplicated(diris)
anyDuplicated(diris$Sepal.Length)

table(duplicated(diris))
table(duplicated(diris$Sepal.Length))
distinct(diris)
distinct(diris, Sepal.Length)
distinct(diris, Sepal.Length, Sepal.Width)

# selecting and subsetting
slice(diris, 1:5)
slice(diris, -(1:145))
tail(diris, 5)

filter(diris, Species == "setosa")
filter(diris, Species != "setosa")
filter(iris, Sepal.Length < 5 & Petal.Width > 0.2)
filter(diris, Sepal.Length == 4.3 | Sepal.Width != 4.4)

interest <- c(4.3, 4.4)
filter(diris, Sepal.Length %in% interest)
filter(diris, !Sepal.Length %in% interest)

filter(diris,
       Sepal.Length == 4.3 | Sepal.Length == 4.4,
       Petal.Width < 0.2)

select(diris, Sepal.Length, Sepal.Width)
select(diris, 1, 5, 2)

select_(diris, "Sepal.Length", "Sepal.Width") # not in use anymore

v <- c("Sepal.Length", "Sepal.Width")
select(diris, one_of(v))
diris[, v]
diris[, 1]

head(diris[["Sepal.Length"]])
select(diris, -1)
select(diris, -Sepal.Length, -Petal.Width)

ex <- paste0("-", v)
select_(diris, ex)
select(diris, -one_of(v))
select(diris, starts_with("s"))
select(diris, starts_with("s", ignore.case = FALSE))

# variable rename and order
names(diris)
colnames(diris)

diris <- rename(diris, SepalLength = Sepal.Length)
names(diris)

diris <- select(diris, SepalLength, Petal.Length, Sepal.Width, Petal.Width,
                Species)
diris

v <- c(1, 3, 2, 4, 5)
diris <- select_(diris, .dots = as.list(v))
diris


# computing on data and creating variables --------------------------------

diris <- mutate(diris, V0=0, X1 = 1L, X2 = 2L)
diris
diris <- select(diris, -V0, -X1, -X2)
diris <- mutate(diris, V = Petal.Length * Petal.Width)
diris

diris <- mutate(diris, V2 = if_else(Species == "setosa", Petal.Length * Petal.Width, NA_real_))
diris <- mutate(diris, V2 = if_else(Species == "virginica",
                                    sqrt(Petal.Length * Petal.Width), V2))
slice(diris, c(1, 51, 101))

summarize(diris, M = mean(Sepal.Width), SD = sd(Sepal.Width))

diris %>% filter(Species == "virginica") %>% 
  summarize(M = mean(Sepal.Width))
diris %>% group_by(Species) %>% 
  summarize(M1 = mean(Sepal.Width),
            M2 = mean(Petal.Width))

diris %>% group_by(Species) %>% 
  summarise(M1 = mean(Sepal.Width), M2 = mean(Petal.Width)) %>% 
  summarise(r = cor(M1, M2))

diris <- diris %>% 
  group_by(Species) %>% 
  mutate(MedPW = Petal.Width > median(Petal.Width))
diris

diris %>% 
  group_by(Species, MedPW) %>% 
  summarise(M1 = mean(Sepal.Width), M2 = mean(Petal.Width))

diris %>% 
  group_by(Species, MedPW) %>% 
  summarise(M1 = mean(Sepal.Width),
            M2 = mean(Petal.Width)) %>% 
  group_by(MedPW) %>% 
  summarise(r = cor(M1, M2))
