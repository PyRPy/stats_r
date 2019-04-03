# Repeated Measures Analysis with R
# https://stats.idre.ucla.edu/r/seminars/repeated-measures-analysis-with-r/

# Demo Analysis #1
# The between groups test indicates that the variable group is significant
# The within subject test indicate that there is not a significant time effect

demo1  <- read.csv("https://stats.idre.ucla.edu/stat/data/demo1.csv")
## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
head(demo1)

par(cex = .6)

with(demo1, interaction.plot(time, group, pulse,
                             ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo1.aov <- aov(pulse ~ group * time + Error(id), data = demo1)
summary(demo1.aov)


# Demo Analysis #2
# the variable group is not significant
# The within subject test indicate that there is a significant time effect
# the interaction is not significant.

demo2 <- read.csv("https://stats.idre.ucla.edu/stat/data/demo2.csv")

## Convert variables to factor
demo2 <- within(demo2, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
par(cex = .6)

with(demo2, interaction.plot(time, group, pulse,
                             ylim = c(10, 40), lty = c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo2.aov <- aov(pulse ~ group * time + Error(id), data = demo2)
summary(demo2.aov)

# Demo Analysis #3
# group - significant
# time - significant
# interaction - significant

demo3 <- read.csv("https://stats.idre.ucla.edu/stat/data/demo3.csv")

## Convert variables to factor
demo3 <- within(demo3, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})

par(cex = .6)

with(demo3, interaction.plot(time, group, pulse,
                             ylim = c(10, 60), lty = c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo3.aov <- aov(pulse ~ group * time + Error(id), data = demo3)
summary(demo3.aov)

# Demo Analysis #4
# interaction time:group is significant
# time ns
# group significant

demo4 <- read.csv("https://stats.idre.ucla.edu/stat/data/demo4.csv")
## Convert variables to factor
demo4 <- within(demo4, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})
par(cex = .6)

with(demo4, interaction.plot(time, group, pulse,
                             ylim = c(10, 60), lty = c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo4.aov <- aov(pulse ~ group * time + Error(id), data = demo4)
summary(demo4.aov)

# --- Exercise data ---
# The data called exer, consists of people who were randomly assigned to two different 
# diets: low-fat and not low-fat and three different types of exercise: at rest, walking 
# leisurely and running. Their pulse rate was measured at three different time points during 
# their assigned exercise: at 1 minute, 15 minutes and 30 minutes.

exer <- read.csv("https://stats.idre.ucla.edu/stat/data/exer.csv")

## Convert variables to factor
exer <- within(exer, {
  diet <- factor(diet)
  exertype <- factor(exertype)
  time <- factor(time)
  id <- factor(id)
})
print(exer)

# Exercise example, model 1 (time and diet)

# plot the trend first
par(cex=.6)

with(exer, interaction.plot(time, diet, pulse,
                            ylim = c(90, 110), lty = c(1, 12), lwd = 3,
                            ylab = "mean of pulse", xlab = "time", trace.label = "group"))


diet.aov <- aov(pulse ~ diet * time + Error(id),
                data = exer)
summary(diet.aov)

# Exercise example, model 2 (time and exercise type)
with(exer, interaction.plot(time, exertype, pulse,
                            ylim = c(80, 130), lty = c(1, 2, 4), lwd = 2,
                            ylab = "mean of pulse", xlab = "time"))

exertype.aov <- aov(pulse ~ exertype * time + Error(id), data = exer)
summary(exertype.aov)

# general steps : plot first, then run aov, check consistencies

# we cannot use this kind of covariance structure in a traditional repeated measures 
# analysis (using the aov function), but we can use it in the gls function.

# Let's look at the correlations, variances and covariances for the exercise data.
mat <- with(exer, matrix(c(pulse[time==1], pulse[time==2], pulse[time==3]), ncol = 3))
mat
var(mat)

cor(mat)

# Exercise example, model 2 using the gls function
# Compound Symmetry
library(nlme)
longg <- groupedData(pulse ~ as.numeric(exertype) * as.numeric(time) | id, data = exer)
fit.cs <- gls(pulse ~ exertype * time, data = longg,
              corr = corCompSymm(, form= ~ 1 | id) )
summary(fit.cs)

# Unstructured
fit.un <- gls(pulse ~ exertype * time, data = longg,
              corr=corSymm(form = ~ 1 | id),
              weights = varIdent(form = ~ 1 | time))
summary(fit.un)

anova(fit.un)

# Autoregressive
fit.ar1 <- gls(pulse ~ exertype * time, data = longg,
               corr = corAR1(, form= ~ 1 | id))
summary(fit.ar1)
anova(fit.ar1)

# Autoregressive with heterogeneous variances
fit.arh1 <- gls(pulse ~ exertype * time, data = longg,
                corr = corAR1(, form = ~ 1 | id), weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)
anova(fit.arh1)

# Model comparison (using the anova function)
anova(fit.cs, fit.un)
anova(fit.cs, fit.ar1)
anova(fit.cs, fit.arh1)

# Exercise example, model 3 (time, diet and exertype)-using the aov function
par(cex = .6)
# Looking at the graphs of exertype by diet.
with(exer, interaction.plot(time[diet==1], exertype[diet==1], pulse[diet==1],
                            ylim = c(80, 150), lty = c(1, 12, 8),
                            trace.label = "exertype", ylab = "mean of pulse", xlab = "time"))
title("Diet = 1")

with(exer, interaction.plot(time[diet==2], exertype[diet==2], pulse[diet==2],
                            ylim = c(80, 150), lty = c(1, 12, 8),
                            trace.label = "exertype", ylab = "mean of pulse", xlab = "time"))
title("Diet = 2")

both.aov <- aov(pulse ~ exertype * diet * time + Error(id), data = exer)
summary(both.aov)

# Exercise example, model 3 (time, diet and exertype)-using the gls fuction
longa <- groupedData(pulse ~ as.numeric(exertype) * as.numeric(diet) * as.numeric(time) | id,
                     data = exer)
both.arh1 <- gls(pulse ~ exertype * diet * time, data = longa,
                 corr = corAR1(, form = ~ 1 | id), weight = varIdent(form = ~ 1 | time))
summary(both.arh1)
anova(both.arh1)

# Contrasts and interaction contrasts for model 3
longa[, c("ef", "df", "tf")] <- longa[, c("exertype", "diet", "time")]

m <- matrix( c( c(-1/2, 1/2, 0), c(-1/3, -1/3, 2/3) ), ncol=2)
contrasts(longa$ef) <- m
(contrasts(longa$tf) <- m)
(contrasts(longa$df)  <- c(-1/2, 1/2))

model.cs <- gls(pulse ~ ef * df * tf, data = longa,
                corr = corCompSymm(, form = ~ 1 | id) )

summary(model.cs)

longa$e1d12 <- (-1/2*(longa$exertype==1 & longa$diet==1))
longa$e1d12[longa$exertype==1 & longa$diet==2] <- 1/2

longa$e2d12 <- (-1/2*(longa$exertype==1))
longa$e2d12[longa$exertype==2 & longa$diet==2] <- 1/2

longa$e3d12 <- (-1/2*(longa$exertype==3 & longa$diet==1))
longa$e3d12[longa$exertype==3 & longa$diet==2] <- 1/2

modela.cs <- gls(pulse ~ ef + e1d12 + e2d12 + e3d12 , data = longa,
                 corr = corCompSymm(, form = ~ 1 | id) )
summary(modela.cs)

# Unequally Spaced Time Points
# Modeling Time as a Linear Predictor of Pulse
study2 <- read.csv("https://stats.idre.ucla.edu/stat/data/study2.csv")
study2 <- within(study2, {
  id <- factor(id)
  exertype <- factor(exertype)
  diet <- factor(diet)
})
study2[1:20, ]

# look at a scatter plot of the data with lines connecting the points for each individual
## Load
library(lattice)

##
par(cex = .6)
xyplot(pulse ~ time, data = study2, groups = id,
       type = "o", panel = panel.superpose)

xyplot(pulse ~ time | exertype, data = study2, groups = id,
       type = "o", panel = panel.superpose)

xyplot(pulse ~ time | diet, data = study2, groups = id,
       type = "o", panel = panel.superpose)

# Since this model contains both fixed and random components, it can be analyzed using 
# the lme function as shown below.
time.linear <- lme(pulse ~ exertype * time,
                   random = list(id = pdDiag(~ time)), data = study2)
summary(time.linear)

fitted <- fitted(time.linear, level=0)

with(study2, plot(time[exertype==3], fitted[exertype==3], ylim = c(50, 150),
                  xlab = "time", ylab = "predicted", type = "b", col = "green"))
with(study2, points(time[exertype==2], fitted[exertype==2],
                    pch = 4, type = "b", col = "red"))
with(study2, points(time[exertype==1], fitted[exertype==1],
                    pch = 16, type = "b", col = "blue"))


xyplot(pulse[exertype==1] ~ time[exertype==1], data = study2, groups = id,
       type = "o", ylim = c(50, 150), xlim = c(0, 800),
       panel = panel.superpose, col = "blue")
with(study2, lines(time[exertype==1], fitted[exertype==1],
                   ylim = c(50, 150),  xlim = c(0, 800),
                   type = "b", col = "dark blue", lwd = 4))

xyplot(pulse[exertype==2] ~ time[exertype==2], data = study2, groups=id,
       type = "o", ylim = c(50, 150), xlim = c(0, 800),
       panel = panel.superpose, col = "red")
with(study2, lines(time[exertype==2], fitted[exertype==2],
                   ylim = c(50, 150),  xlim = c(0, 800),
                   type = "b", col = "dark red", lwd = 4))

xyplot(pulse[exertype==3] ~ time[exertype==3], data = study2, groups = id,
       type = "o", ylim = c(50, 150), xlim = c(0, 800),
       panel = panel.superpose, col = "green")
with(study2, lines(time[exertype==3], fitted[exertype==3],
                   ylim = c(50, 150), xlim = c(0, 800),
                   type = "b", col = "dark green", lwd = 4))

# Modeling Time as a Quadratic Predictor of Pulse
study2$time2 <- study2$time^2
time.quad <- lme(pulse ~ exertype * time + time2,
                 random = list(id = pdDiag(~ time)), study2)
summary(time.quad)
anova(time.quad)

fitted2 <- fitted(time.quad, level = 0)
a <- with(study2,
          data.frame(time, fitted2, exertype)[order(exertype, time), ])

with(a, {
  plot(time[exertype==3], fitted2[exertype==3], ylim = c(50, 150),
       xlab = "time", ylab = "predicted", col = "green", type = "b")
  points(time[exertype==2], fitted2[exertype==2],
         pch = 4, col = "red", type = "b")
  points(time[exertype==1], fitted2[exertype==1],
         pch = 16, col = "blue", type = "b")
  title("Time Quadratic Effect")})

# Modeling Time as a Quadratic Predictor of Pulse, Interacting by Exertype
time.quad2 <- lme(pulse ~ exertype * time + exertype * time2,
                  random = list(id = pdDiag(~ time)), data = study2)
summary(time.quad2)
anova(time.quad2)

fitted3 <- fitted(time.quad2, level = 0)
a <- with(study2,
          data.frame(time, fitted3, exertype)[order(exertype, time), ])

with(a, {
  plot(time[exertype==3], fitted3[exertype==3], ylim = c(50, 150),
       xlab = "time", ylab = "predicted", col = "green", type = "b")
  points(time[exertype==2], fitted3[exertype==2],
         pch = 4, col = "red", type = "b")
  points(time[exertype==1], fitted3[exertype==1],
         pch = 16,  col = "blue", type = "b")
  title("Time Quadratic Effect")})

