# Repeated Measures ANOVA -------------------------------------------------
# https://rcompanion.org/handbook/I_09.html
# Packages used in this chapter
if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}

# Repeated measures ANOVA example
Input = ("
Instruction        Student  Month   Calories.per.day
         'Curriculum A'     a        1       2000
         'Curriculum A'     a        2       1978
         'Curriculum A'     a        3       1962
         'Curriculum A'     a        4       1873
         'Curriculum A'     a        5       1782
         'Curriculum A'     a        6       1737
         'Curriculum A'     b        1       1900
         'Curriculum A'     b        2       1826
         'Curriculum A'     b        3       1782
         'Curriculum A'     b        4       1718
         'Curriculum A'     b        5       1639
         'Curriculum A'     b        6       1644
         'Curriculum A'     c        1       2100
         'Curriculum A'     c        2       2067
         'Curriculum A'     c        3       2065
         'Curriculum A'     c        4       2015
         'Curriculum A'     c        5       1994
         'Curriculum A'     c        6       1919
         'Curriculum A'     d        1       2000
         'Curriculum A'     d        2       1981
         'Curriculum A'     d        3       1987
         'Curriculum A'     d        4       2016
         'Curriculum A'     d        5       2010
         'Curriculum A'     d        6       1946
         'Curriculum B'     e        1       2100
         'Curriculum B'     e        2       2004
         'Curriculum B'     e        3       2027
         'Curriculum B'     e        4       2109
         'Curriculum B'     e        5       2197
         'Curriculum B'     e        6       2294
         'Curriculum B'     f        1       2000
         'Curriculum B'     f        2       2011
         'Curriculum B'     f        3       2089
         'Curriculum B'     f        4       2124
         'Curriculum B'     f        5       2199
         'Curriculum B'     f        6       2234
         'Curriculum B'     g        1       2000
         'Curriculum B'     g        2       2074
         'Curriculum B'     g        3       2141
         'Curriculum B'     g        4       2199
         'Curriculum B'     g        5       2265
         'Curriculum B'     g        6       2254
         'Curriculum B'     h        1       2000
         'Curriculum B'     h        2       1970
         'Curriculum B'     h        3       1951
         'Curriculum B'     h        4       1981
         'Curriculum B'     h        5       1987
         'Curriculum B'     h        6       1969
         'Curriculum C'     i        1       1950
         'Curriculum C'     i        2       2007
         'Curriculum C'     i        3       1978
         'Curriculum C'     i        4       1965
         'Curriculum C'     i        5       1984
         'Curriculum C'     i        6       2020
         'Curriculum C'     j        1       2000
         'Curriculum C'     j        2       2029
         'Curriculum C'     j        3       2033
         'Curriculum C'     j        4       2050
         'Curriculum C'     j        5       2001
         'Curriculum C'     j        6       1988
         'Curriculum C'     k        1       2000
         'Curriculum C'     k        2       1976
         'Curriculum C'     k        3       2025
         'Curriculum C'     k        4       2047
         'Curriculum C'     k        5       2033
         'Curriculum C'     k        6       1984
         'Curriculum C'     l        1       2000
         'Curriculum C'     l        2       2020
         'Curriculum C'     l        3       2009
         'Curriculum C'     l        4       2017
         'Curriculum C'     l        5       1989
         'Curriculum C'     l        6       2020
         ")

Data = read.table(textConnection(Input),header=TRUE)

###  Order factors by the order in data frame
###  Otherwise, R will alphabetize them

Data$Instruction = factor(Data$Instruction,
                          levels=unique(Data$Instruction))


###  Check the data frame

library(psych)

headTail(Data)

str(Data)

summary(Data)

### Remove unnecessary objects

rm(Input)

# Define model and conduct analysis of deviance
library(nlme)

model = lme(Calories.per.day ~ Instruction + Month + Instruction*Month,
            random = ~1|Student,
            correlation = corAR1(form = ~ Month | Student, value = 0.4287),
            data=Data,
            method="REML")

library(car)

Anova(model)

# Test the random effects in the model
model.fixed = gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
                  data=Data,
                  method="REML")

anova(model, model.fixed)

# p-value and pseudo R-squared for model
# not working
library(rcompanion)

model.null.2 = gls(Calories.per.day ~ 1,
                   data = Data)

nagelkerke(model, model.null.2)

library(multcompView)

library(lsmeans)

marginal = lsmeans(model,
                   ~ Instruction:Month)

cld(marginal,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons

# Interaction plot
SUM <- read.csv("data_sum.csv")
head(SUM)
library(ggplot2)

pd = position_dodge(.2)

ggplot(SUM, aes(x =    Month,
                y =    Mean,
                color = Instruction)) +
  geom_errorbar(aes(ymin=Percentile.lower,
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean calories per day")

# Histogram of residuals
x = residuals(model)

hist(x)
plot(fitted(model),
     residuals(model))

# Optional analysis: determining autocorrelation in residuals
library(nlme)

model.a = gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
              data=Data)

ACF(model.a,
    form = ~ Month | Student)

model.b = lme(Calories.per.day ~ Instruction + Month + Instruction*Month,
              random = ~1|Student,
              data=Data)

ACF(model.b)

