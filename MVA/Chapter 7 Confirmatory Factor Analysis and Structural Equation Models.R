# 7 Confirmatory Factor Analysis and Structural Equation Models
library(MVA)
library(sem)
# 7.3 Confirmatory factor analysis models ---------------------------------

# 7.3.2 A confirmatory factor analysis model for drug use
head(druguse)
druguse_model <- specify.model(file = "druguse_model.txt")
druguse_sem <- sem(druguse_model, druguse, 1634)

summary(druguse_sem)
round(druguse_sem$S - druguse_sem$C, 3)

path.diagram(druguse_sem, file = "druguse_sem", 
                           ignore.double = FALSE, edge.labels = "both") 
# 7.4 Structural equation models ------------------------------------------


