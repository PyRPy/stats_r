#BIOL933, Lab 8
#Example 1

#read in, re-classify, and inspect the data
fact_dat <- read.csv("Data/Lab8ex1.csv")

fact_dat<-as.data.frame(fact_dat)
fact_dat$A<-as.factor(fact_dat$A)
fact_dat$B<-as.factor(fact_dat$B)
fact_dat$C<-as.factor(fact_dat$C)
str(fact_dat, give.attr = F)

#The ANOVA 
fact_mod<-lm(Y ~ (A + B + C)^2, fact_dat)
anova(fact_mod)

d <- anova(fact_mod)$Df
M <- anova(fact_mod)$'Mean Sq'

F_A_num <- M[1]+M[7]
F_A_den <- M[4]+M[5]
df_A_num <- F_A_num^2 / ((M[1]^2/d[1]) + (M[7]^2/d[7]))
df_A_den <- F_A_den^2 / ((M[4]^2/d[4]) + (M[5]^2/d[5]))
p_A <- pf(F_A_num/F_A_den, df_A_num, df_A_den, lower.tail = FALSE)
p_A # 0.6707082

F_B_num <- M[2]+M[7]
F_B_den <- M[4]+M[6]
df_B_num <- F_B_num^2 / ((M[2]^2/d[2]) + (M[7]^2/d[7]))
df_B_den <- F_B_den^2 / ((M[4]^2/d[4]) + (M[6]^2/d[6]))
p_B <- pf(F_B_num/F_B_den, df_B_num, df_B_den, lower.tail = FALSE)
p_B # 0.7364489

F_C_num <- M[3]+M[7]
F_C_den <- M[5]+M[6]
df_C_num <- F_C_num^2 / ((M[3]^2/d[3]) + (M[7]^2/d[7]))
df_C_den <- F_C_den^2 / ((M[5]^2/d[5]) + (M[6]^2/d[6]))
p_C <- pf(F_C_num/F_C_den, df_C_num, df_C_den, lower.tail = FALSE)
p_C # 0.3454594
# all NS