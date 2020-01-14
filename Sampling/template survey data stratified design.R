# template analyze survey data with stratified design
library(survey)

mydata <-
  read.table( text =
    "id str clu wt hou85 ue91 lab91
		1 2 1  0.5    26881 4123 33786
		2 2 1  0.5    26881 4123 33786
		3 1 10 1.004  9230  1623 13727
		4 1 4  1.893  4896  760  5919
		5 1 7  2.173  4264  767  5823
		6 1 32 2.971  3119  568  4011
		7 1 26 4.762  1946  331  2543
		8 1 18 6.335  1463  187  1448
		9 1 13 13.730 675   129  927" ,
    header = TRUE
  )

head(mydata)

class(mydata)

ncol(mydata)

summary(mydata$ue91) # variable of interest

# formulate a survey design
mydesign <- svydesign(id = ~ clu,
                      data = mydata,
                      weight = ~ wt,
                      strata = ~ str)

# veiw survey design structure
mydesign
class(mydesign)

length(unique(mydata$clu))
length(unique(mydata$str))

degf(mydesign)
unwtd.count(~ ue91, mydesign)

svymean(~ue91, mydesign)

options(survey.lonely.psu = 'adjust')
svymean(~ue91, mydesign)

options(survey.lonely.psu = 'certainty')
svymean(~ue91, mydesign)
mysvymean <- svymean(~ue91, mydesign, deff = TRUE)

coef(mysvymean)
SE(mysvymean)
deff(mysvymean)
confint(mysvymean)

(myratio <- svyratio(~ue91, ~lab91, mydesign))
confint(myratio)

svyquantile(~ue91, mydesign, quantiles = 0.5, ci = TRUE)

















