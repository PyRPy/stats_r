# ANOVA vs Multiple Comparisons -------------------------------------------
# https://www.r-bloggers.com/2020/10/anova-vs-multiple-comparisons/
library(multcomp)
library(tidyverse)
# Create the four groups
set.seed(10) 
df1 <- data.frame(Var="a", Value=rnorm(100,10,5))
df2 <- data.frame(Var="b", Value=rnorm(100,10,5))
df3 <- data.frame(Var="c", Value=rnorm(100,11,6))
df4 <- data.frame(Var="d", Value=rnorm(100,11,6))

# merge them in one data frame
df<-rbind(df1,df2,df3,df4)
# convert Var to a factor
df$Var<-as.factor(df$Var)
df%>%ggplot(aes(x=Value, fill=Var))+geom_density(alpha=0.5)

# ANOVA
model1<-lm(Value~Var, data=df)
anova(model1)

# Tukey multiple comparisons
summary(glht(model1, mcp(Var="Tukey")))

# t-test for var a vs var c
t.test(df%>%filter(Var=="a")%>%pull(), df%>%filter(Var=="c")%>%pull())

# t-test for b vs c
t.test(df%>%filter(Var=="b")%>%pull(), df%>%filter(Var=="c")%>%pull())