# soap.r, soap experiment, Tables 3.10 and 3.11, pages 60 and 63

# Table 3.10, p60
# Read the data into the data.frame "soap.data"
soap.data <- read.table("Data/soap.txt", header = TRUE)
head(soap.data, 5)  # Display first 5 lines of soap.data

# Add factor variable fSoap to soap.data frame for later ANOVA
soap.data$fSoap <- factor(soap.data$Soap)

# Plot WtLoss vs Soap, specify axis labels, suppress x-axis.
plot(WtLoss ~ Soap, data=soap.data, 
     ylab = "Weight Loss (grams)", las=1, xaxt = "n")
# Insert x-axis (axis 1) with tick marks from 1 to 3 by 1.
axis(1, at = seq(1, 3, 1)) 

# Table 3.11, p63
summary(soap.data[,c(1,5:6)]) # Summarize data in cols 1, 5, 6
model1 <- aov(WtLoss ~ fSoap, data=soap.data)
anova(model1)

# install.packages("lsmeans")
# The lsmeans command generates the least squares means for the three
# levels of the factor fSoap
library(lsmeans)
lsmeans(model1, "fSoap")

# use ggplot for categorical factors
library(tidyverse)
library(ggplot2)
soap.data %>% ggplot(aes(x=fSoap, y=WtLoss, color=fSoap)) +
  geom_point()

dev.off()
