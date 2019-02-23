# voltage.r, voltage experiment, Tables 18.8, pp693 

voltage.data <- read.table("voltage.txt", header=T) 
voltage.data <- within(voltage.data, {fSetting = factor(Setting); 
               fRegul = factor(Regul); fTesting = factor(Testing) })
head(voltage.data, 3)

# Least squares ANOVA
# Set contrast options for correct lsmeans and contrasts
options(contrasts = c("contr.sum", "contr.poly"))
model1 <- aov(Voltg ~ fSetting + fTesting + Error(fSetting:fRegul), 
              data=voltage.data)
summary(model1)

# Multiple comparisons: Tukey's method
library(lsmeans) 
lsmTesting1 <- lsmeans(model1, ~ fTesting) 
summary(contrast(lsmTesting1, method="pairwise", adjust="tukey"), 
        infer=c(T,T), level=0.98, side="two-sided")

### Multiple comparisons for Setting (not in Table 18.8)
lsmSetting1 <- lsmeans(model1, ~ fSetting) 
summary(contrast(lsmSetting1, method="pairwise", adjust="tukey"), 
        infer=c(T,T), level=0.98, side="two-sided")