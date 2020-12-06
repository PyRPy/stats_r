# An example with linear regression ---------------------------------------

rm(list=ls())
library(nlme)
library(lattice)
dataset <- read.csv("https://raw.githubusercontent.com/OnofriAndreaPG/agroBioData/master/yieldDensityB.csv",
                    header=T)
dataset$block <- factor(dataset$block)
head(dataset)

plot(yield ~ density, data = dataset)

mod.reg <- lm(yield ~ block + density, data=dataset)
summary(mod.reg)

mod.reg2 <- lm(yield ~ block/density + block, data=dataset)
anova(mod.reg, mod.reg2)

# take the block effect as random
modMix.1 <- lme(yield ~ density, random = ~ density|block, data=dataset)
summary(modMix.1)

modMix.1 <- lme(yield ~ density, random = list(block = pdSymm(~density)), 
                data=dataset)
modMix.2 <- lme(yield ~ density, random = list(block = pdDiag(~density)), 
                data=dataset)
summary(modMix.2)

anova(modMix.1, modMix.2)

# The model could be further simplified
# Model with only random intercept
modMix.3 <- lme(yield ~ density, random = list(block = ~1), data=dataset)

#Alternative
#random = ~ 1|block

# Model with only random slope
modMix.4 <- lme(yield ~ density, random = list(block = ~ density - 1), 
                data=dataset)

#Alternative
#random = ~density - 1 | block


# An example with nonlinear regression ------------------------------------
rm(list=ls())
dataset <- read.csv("https://raw.githubusercontent.com/OnofriAndreaPG/agroBioData/master/YieldLossB.csv",
                    header=T)
dataset$block <- factor(dataset$block)
head(dataset)

plot(yieldLoss ~ density, data = dataset)

# library(devtools)
# install_github("onofriandreapg/aomisc")
library(aomisc)
datasetG <- groupedData(yieldLoss ~ 1|block, dataset)
nlin.mix <- nlme(yieldLoss ~ NLS.YL(density, i, A), data=datasetG, 
                 fixed = list(i ~ 1, A ~ 1),
                 random = i + A ~ 1|block)
summary(nlin.mix)

nlin.mix2 <- nlme(yieldLoss ~ NLS.YL(density, i, A), data=datasetG, 
                  fixed = list(i ~ 1, A ~ 1),
                  random = pdSymm(list(i ~ 1, A ~ 1)))
summary(nlin.mix2)

nlin.mix3 <- nlme(yieldLoss ~ NLS.YL(density, i, A), data=datasetG, 
                  fixed = list(i ~ 1, A ~ 1),
                  random = pdDiag(list(i ~ 1, A ~ 1)))
summary(nlin.mix3)

# https://www.r-bloggers.com/2020/12/accounting-for-the-experimental-design-in-linear-nonlinear-regression-analyses/
