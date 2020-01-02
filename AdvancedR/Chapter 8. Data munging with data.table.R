# chapter 8. Data munging with data.table

library(stringdist)
library(data.table)
library(foreign)
options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20,
        datatable.print.topn = 3,
        digits = 2)


# data munging and cleaning -----------------------------------------------
d <- read.dta("04691-0001-Data.dta")
d <- as.data.table(d)
setkey(d, IDNUMR)
str(d, give.attr=FALSE, strict.width="cut", list.len=20)

table(d[, EDUCATIO])
table(d[, PLANGUAG])
table(d[, BMICLASS])
table(d[, S1Q01])

# recoding data
grep(pattern = "abc", x = c("a", "abc", "def", "abc", "d"))
grepl(pattern = "abc", x = c("a", "abc", "def", "abc", "d"))

grep(pattern = "REFUSED",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - REFUSED"),
     value = TRUE)

grep(pattern = "- REFUSED",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - REFUSED"),
     value = TRUE)

# as the last part of the string
grep(pattern = "- REFUSED$",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - REFUSED"),
     value = TRUE)

# numbers before string + means previous expression occurs one | more times
grep(pattern = "[0-9]+ - REFUSED$",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - JACK - REFUSED", "4 - REFUSED", "97 - REFUSED", 
           "- REFUSED"),
     value = TRUE)

# start with using ^
grep(pattern = "^[-]*[0-9]+ - REFUSED$",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - JACK - REFUSED", "4 - REFUSED", "97 - REFUSED", "-97 - REFUSED",
           "- REFUSED", "TRICK CASE 4 - REFUSED"),
     value = TRUE)

# pipe operator |
grep(pattern = "^[-]*[0-9]+ - REFUSED$|MISSING$",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - JACK - REFUSED", "4 - REFUSED", "97 - REFUSED", "-97 - REFUSED",
            "-2 - MISSING", "- REFUSED", "TRICK CASE 4 - REFUSED"),
     value = TRUE)

# becoming complex
p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIP$|PARTIAL INTERVIEW|NOT IN UNIVERSE"

grep(pattern = p, 
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - JACK - REFUSED", "4 - REFUSED", "97 - REFUSED", "-97 - REFUSED",
           "-2 - MISSING", 
           "96 - DON'T KNOW",
           "-4 - LEGITIMATE SKIP",
           "-3 - PARTIAL INTERVIEW",
           "-2 - NOT IN UNIVERSE",
           "- REFUSED", 
           "TRICK CASE 4 - REFUSED",
           "4 - PARTICAL INTERVIEW OF DOCTOR"),
     value = TRUE)

v <- c("EDUCATIO", "PLANGUAG", "BMICLASS", "S1Q01", "S2Q01")
d[, (v):= lapply(.SD, function(x){
  x[grep(pattern = p, x)] <- NA
  if (is.factor(x)) droplevels(x) else x }), .SDcols = v]

table(d[, EDUCATIO])
table(d[, S1Q01])

# gsub for replacement
gsub(pattern = "abc", replacement = "", x = c("a", "abcd", "123abc456"))

p.remove <- "^[-]*[0-9]+ - "
gsub(pattern = p.remove, replacement ="",
     x = c("1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT",
           "3 - JACK - REFUSED", "4 - REFUSED", "97 - REFUSED", "-97 - REFUSED",
           "-2 - MISSING", 
           "96 - DON'T KNOW",
           "-4 - LEGITIMATE SKIP",
           "-3 - PARTIAL INTERVIEW",
           "-2 - NOT IN UNIVERSE",
           "- REFUSED", 
           "TRICK CASE 4 - REFUSED",
           "4 - PARTICAL INTERVIEW OF DOCTOR")
     )

d <- read.dta("04691-0001-Data.dta")
d <- as.data.table(d)
setkey(d, IDNUMR)

d[, (v):=lapply(.SD, function(x){
  f <- is.factor(x)
  x[grepl(pattern = p, x)] <- NA
  x <- gsub(pattern = p.remove, replacement = "", x)
  if(f) factor(x) else x}), .SDcols = v]

table(d[, EDUCATIO])

table(d[, S1Q01])

table(d[, S2Q01])

# recoding numberic values
table(d[!S2Q02R %between% c(0, 90), S2Q02R])
table(d[!S2Q03R %between% c(0, 900), S2Q03R])

hist(d[,.(S2Q03R)][[1]]) # get the data
hist(d[, .(S2Q02R)][[1]])
d[, .(S2Q02R)][[1]]

v2 <- c("S2Q02R", "S2Q03R", "AGEYR_CH")
m <- sort(c(9, 99, 999))
for (k in v2){
  j <- i <- 1
  while(j == 1 & i <= length(m)){
    if(max(d[[k]], na.rm = TRUE) < m[i]){
      j <- 0
      d[!(get(k) %between% c(0, ifelse(m[i] > 90,
      m[i] - 9, m[i] - 1e-9))), (k):= NA_integer_]
    } else {i <- i + 1}
  }
}

hist(d[,.(S2Q03R)][[1]]) # get the data
hist(d[, .(S2Q02R)][[1]], bin=60)


# creating new variables --------------------------------------------------

v.health <- paste0("S2Q", c(19, 20, 21, 22, 23, 24, 26, 35, 37))
v.health
table(unlist(d[, v.health, with=FALSE]))

p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIPS$|PARTIAL INTERVIEW$|NOT IN UVIVERSE$"

d[, (v.health):=lapply(.SD, function(x){
  x[grepl(pattern = p, x)] <- NA
  if(is.factor(x)) droplevels(x) else x
}), .SDcols = v.health]

table(unlist(d[, v.health, with=FALSE]), useNA = "ifany")

# reduce function
Reduce('+', c(1, 2, 3))
Reduce('+', list(1:3, 4:6, 7:9)) # like column addition
Reduce('/', list(1:3, 4:6, 7:9))
Reduce('^', list(1:3, 4:6, 3:1))

fplus <- function(e1, e2){
  if(is.factor(e1)){
    e1 <- as.numeric(e1) -1
  }
  if (is.factor(e2)){
    e2 <- as.numeric(e2) - 1
  }
  e1 + e2
}

d[, NHealthConditions2:=Reduce(fplus, .SD), .SDcols = v.health]
table(d$NHealthConditions2)

# fuzzy matching