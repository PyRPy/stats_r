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
           "TRICK CASE 4 - REFUSED"),
           "4 - PARTICAL INTERVIEW OF DOCTOR",
     value = TRUE)
