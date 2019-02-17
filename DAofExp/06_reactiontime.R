# reactiontime.r, reaction time experiment, Table 6.15, page 185, plus 
# Dunnett's method for all treatment versus control comparisons, text p189

# Table 6.15, page 185
react.data <- read.table("Data/reaction.time.txt", header=T)
react.data
react.data <- head(react.data, 14) # Keep first 14 observations
head(react.data, 3)

# Create trtmt combo vbl TC and factors fTC, fA, and fB within data set
react.data <- within(react.data,
  {TC = 10*A + B; fTC = factor(TC); fA = factor(A); fB = factor(B)})

head(react.data)
summary(react.data[,c("fA","fB","fTC","y")])

# ANOVA
options(contrasts = c("contr.sum", "contr.poly"))
modelAB <- aov(y ~ fA + fB + fA:fB, data = react.data)

anova(modelAB) # Type I ANOVA
drop1(modelAB, ~., test = "F") # Type III ANOVA

modelTC <- aov(y ~ fTC, data = react.data)
anova(modelTC) # Model F-test

# Contrasts: estimates, CIs, tests
library(lsmeans)

# Main-effect-of-B contrast: B1-B2
lsmB <- lsmeans(modelAB, ~ fB)
summary(contrast(lsmB, list(B12=c( 1,-1, 0))), infer=c(T,T))

# AB-interaction contrast: AB11-AB13-AB21+AB23
lsmAB <- lsmeans(modelAB, ~ fB:fA) # Using "fB:fA" yields AB lex order
lsmAB # Display to see order of AB combos for contrast coefficients

summary(contrast(lsmAB, list(AB=c( 1 ,0,-1,-1, 0, 1))), infer=c(T,T))

# Multiple comparisons: B
confint(lsmB, level=0.95) # lsmeans for B and 99% CIs

# Tukey's method
summary(contrast(lsmB, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95)

# Dunnett's method
summary(contrast(lsmB, method="trt.vs.ctrl", adj="mvt", ref=1),
        infer=c(T,T), level=0.95)

# Dunnett's method for all treatment versus control comparisons, text page 189
lsmTC <- lsmeans(modelTC, ~ fTC)
summary(contrast(lsmTC, method="trt.vs.ctrl", adj="mvt"),
        infer=c(T,T), level=0.95, side=">")

plot(y ~ A, data=react.data, xaxt="n", type="n") # Suppress x-axis, pts
axis(1, at=seq(1,2,1)) # Add x-axis with tick marks from 1 to 2 by 1
text(y ~ A, B, cex=0.75, data=react.data) # Plot z vs A using B label
mtext("B=1,2,3", side=3, adj=1, line=1) # Margin text, top-rt, line 1
abline(h=0) # Horizontal line at zero

interaction.plot(x.factor = react.data$fA, trace.factor = react.data$fB,
                 response = react.data$y, type ="b",
                 xlab ="A", trace.label ="B", ylab ="Mean of y")

plot(modelAB$res ~ react.data$TC, xlab ="AB", ylab ="Residual")
plot(react.data$y ~ react.data$TC, xlab ="AB", ylab ="y")

plot(modelAB$res ~ react.data$Trtmt, xaxt="n", xlab="AB", ylab="Residual")
axis(1, at = react.data$Trtmt, labels = react.data$fTC)
plot(react.data$y ~ react.data$Trtmt, xaxt="n", xlab="AB", ylab="y")
axis(1, at = react.data$Trtmt, labels = react.data$fTC)

by(modelAB$res, react.data$A, var)
