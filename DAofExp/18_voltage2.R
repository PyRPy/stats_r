# voltage2.r, voltage experiment, Table 18.9, p696 

voltage.data <- read.table("voltage.txt", header=T) 
voltage.data <- within(voltage.data, {fSetting = factor(Setting); 
               fRegul = factor(Regul); fTesting = factor(Testing) })
head(voltage.data)

# Drop two outliers, then reanalyze the data
voltage2.data <- subset(voltage.data, 
                         !(fSetting == 2 & fRegul == 2 & fTesting == 1)
                      &  !(fSetting == 5 & fRegul == 3 & fTesting == 2) )

# REML
# install.packages("lmerTest")
library(lmerTest) # Attaches/masks lmer and lsmeans, 
                  # adding p-values to anova()
model2 <- lmer(Voltg ~ fSetting + fTesting + (1|fSetting:fRegul),
                  data=voltage2.data)
anova(model2) # F-tests for fixed effects

# Multiple comparisons
library(lsmeans) 
lsmTesting2 <- lsmeans(model2, ~ fTesting) 
summary(contrast(lsmTesting2, method="pairwise", adjust="tukey"), 
        infer=c(T,T), level=0.98, side="two-sided")
lsmSetting2 <- lsmeans(model2, ~ fSetting) 
summary(contrast(lsmSetting2, method="pairwise", adjust="tukey"), 
        infer=c(T,T), level=0.98, side="two-sided")