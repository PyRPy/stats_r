# airvelocity.r, air velocity experiment, Table 6.20, p192

air.data <- read.table("Data/air.velocity.contrasts.txt", header=T)
air.data

# Fit linear regression model, save as model1
model1 <- lm(y ~ Aln + Aqd + Bln + Bqd + Bcb + Bqr + Bqn
                    + Aln:Bln + Aln:Bqd + Aln:Bcb + Aln:Bqr
                    + Aqd:Bln + Aqd:Bqd + Aqd:Bcb, data=air.data)
# ANOVA
anova(model1)
