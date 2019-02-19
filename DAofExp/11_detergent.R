# detergent.r, detergent experiment, Table 11.22, p385

detrgnt.data <- read.table("Data/detergent.txt", header=T)
head(detrgnt.data)

detrgnt.data <- within(detrgnt.data,
  {fBlock = factor(Block); fTrtmt = factor(Trtmt) })

# Plot y vs Trtmt using block level as plotting symbol.
plot(y ~ Trtmt, data=detrgnt.data, xaxt="n", type="n") # Suppress x-axis, pts
  axis(1, at=seq(1,9,1)) # x-axis labels 1:9
  text(y ~ Trtmt, Block, cex=0.75, data=detrgnt.data) # Plot y*Trtmt=Block
  mtext("Block=1,...,12", side=3, adj=1, line=1) # Margin text, TopRt, line 1

# Analysis of variance
model1 <- lm(y ~ fBlock + fTrtmt, data=detrgnt.data)
anova(model1)
drop1(model1, ~., test="F")

# Contrast estimates
library(lsmeans)
lsmTrtmt <- lsmeans(model1, ~ fTrtmt)
lsmTrtmt
cntrsts <- summary(contrast(lsmTrtmt,
                            list(I.linear=c(-3,-1, 1, 3, 0, 0, 0, 0, 0),
                                   I.quad=c( 1,-1,-1, 1, 0, 0, 0, 0, 0),
                                  I.cubic=c(-1, 3,-3, 1, 0, 0, 0, 0, 0),
                                II.linear=c( 0, 0, 0, 0,-3,-1, 1, 3, 0),
                                  II.quad=c( 0, 0, 0, 0, 1,-1,-1, 1, 0),
                                 II.cubic=c( 0, 0, 0, 0,-1, 3,-3, 1, 0),
                                  I.vs.II=c( 1, 1, 1, 1,-1,-1,-1,-1, 0),
                              Trt.vs.Ctrl=c( 1, 1, 1, 1, 1, 1, 1, 1,-8))),
                    infer=c(T,T))

# Compute and include contrast sums of squares: ss=t^2*mse
mse<- anova(model1)[3,3]
mse
cntrsts <- cbind(cntrsts, ss=cntrsts[,"t.ratio"]^2*mse)
options(width=75, digits=3, scipen=2)
cntrsts
# options(ooptions)

# Dunnett's method
summary(contrast(lsmTrtmt, method="trt.vs.ctrl", adjust="mvt", ref=9),
        infer=c(T,T))
