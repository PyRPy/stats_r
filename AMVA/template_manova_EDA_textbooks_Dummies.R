# MANOVA for textbooks ----------------------------------------------------
# https://www.dummies.com/education/math/statistics/statistical-analysis-r-dummies/


# MANOVA in R -------------------------------------------------------------

textbooks <- read.csv("textbooks.csv")
head(textbooks)
str(textbooks)
textbooks$Book <- as.factor(textbooks$Book)

m.analysis <- manova(cbind(Physics,Chemistry,Biology) ~ Book, 
                     data = textbooks)

summary(m.analysis)
summary(m.analysis, test = "Roy")


# Visualizing the MANOVA results ------------------------------------------
library(reshape2)
library(ggplot2)
textbooks_melt = melt(textbooks)
head(textbooks_melt)
colnames(textbooks_melt) = c("Student", "Book", "Science", "Score")

ggplot(textbooks_melt,(aes(x=Science,y=Score)))+
  stat_boxplot(geom="errorbar", width =.5) +
  geom_boxplot() +
  facet_grid(. ~ Book)


# After the analysis ------------------------------------------------------

summary.aov(m.analysis)
