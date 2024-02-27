
# 19. CAUSAL INFERENCE USING DIRECT REGRESSION ----------------------------
# https://avehtari.github.io/ROS-Examples/ElectricCompany/electric.html
library("rstanarm")
invlogit <- plogis

# data
electric_wide <- read.table("ROS_Data/electric_wide.txt", header=TRUE)
head(electric_wide)

# Linear model
attach(electric_wide)
post_test <- c(treated_posttest, control_posttest)
pre_test <- c(treated_pretest, control_pretest)
grade <- rep(electric_wide$grade, 2)
treatment <- rep(c(1,0), rep(length(treated_posttest),2))
supp <- rep(NA, length(treatment))
n_pairs <- nrow(electric_wide)
pair_id <- rep(1:n_pairs, 2)
supp[treatment==1] <- ifelse(supplement=="Supplement", 1, 0)
n <- length(post_test)
electric <- data.frame(post_test, pre_test, grade, treatment, supp, pair_id)
#write.csv(electric, root("ElectricCompany/data","electric.csv"))
#electric <- read.csv(root("ElectricCompany/data","electric.csv"))
detach(electric_wide)

fit_3 <- stan_glm(post_test ~ treatment + pre_test + treatment:pre_test,
                  subset=(grade==4), data=electric, refresh = 0)
print(fit_3)
#                    Median MAD_SD
# (Intercept)        38.5    5.0
# treatment          14.5    9.1
# pre_test            0.7    0.0
# treatment:pre_test -0.1    0.1

# for subset grade 4
fit_4 <- stan_glm(post_test ~ treatment + pre_test + treatment * pre_test,
                  subset = (grade==4), data=electric, refresh = 0)
sim_4 <- as.matrix(fit_4)

# Mean effect
n_sims <- 1000
effect <- array(NA, c(n_sims, sum(grade==4)))
for (i in 1:n_sims)
  effect[i,] <- sim_4[i,2] + sim_4[i,4]*pre_test[grade==4]
mean_effect <- rowMeans(effect)

# Plot repeated regression results
est1 <- rep(NA,4)
est2 <- rep(NA,4)
se1 <- rep(NA,4)
se2 <- rep(NA,4)
for (k in 1:4) {
  fit_1 <- stan_glm(post_test ~ treatment, subset=(grade==k), data = electric,
                    refresh = 0, save_warmup = FALSE,
                    open_progress = FALSE, cores = 1)
  fit_2 <- stan_glm(post_test ~ treatment + pre_test, subset=(grade==k),
                    data = electric, refresh = 0, save_warmup = FALSE,
                    open_progress = FALSE, cores = 1)
  est1[k] <- coef(fit_1)[2]
  est2[k] <- coef(fit_2)[2]
  se1[k] <- se(fit_1)[2]
  se2[k] <- se(fit_2)[2]
}
regression.2tables <- function (name, est1, est2, se1, se2, label1, label2, file, bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est1+2*se1, est1-2*se1, est2+2*se2, est1-2*se2)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .6*J
  width <- 8*(name.range+1)
  gap <- .4

  if (!is.na(file)) postscript(file, horizontal=F, height=height, width=width)
  par (mar=c(0,0,0,0))
  plot (c(-name.range,2+gap), c(3,-J-2), bty="n", xlab="", ylab="",
        xaxt="n", yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Subpopulation", adj=0, cex=1)
  text (.5, 2, label1, adj=.5, cex=1)
  text (1+gap+.5, 2, label2, adj=.5, cex=1)
  lines (c(0,1), c(0,0))
  lines (1+gap+c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1), lty=2, lwd=.5)
  lines (1+gap+c(A,A), c(0,-J-1), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  segments (1+gap+A + B*ax, -.1, 1+gap+A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=1)
  text (1+gap+A + B*ax, .7, ax, cex=1)
  text (-name.range, -(1:J), name, adj=0, cex=1)
  points (A + B*est1, -(1:J), pch=20, cex=1)
  points (1+gap+A + B*est2, -(1:J), pch=20, cex=1)
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (1+gap+A + B*(est2-se2), -(1:J), 1+gap+A + B*(est2+se2), -(1:J), lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  segments (1+gap+A + B*(est2-2*se2), -(1:J), 1+gap+A + B*(est2+2*se2), -(1:J), lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1))
    lines (1+gap+c(0,1), c(-J-1,-J-1))
    segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    segments (1+gap+A + B*ax, -J-1-.1, 1+gap+A + B*ax, -J-1+.1, lwd=.5)
    text (A + B*ax, -J-1-.7, ax, cex=1)
    text (1+gap+A + B*ax, -J-1-.7, ax, cex=1)
  }
  if (!is.na(file)) graphics.off()
}

regression.2tables(paste("Grade", 1:4), est1, est2, se1, se2,
                   "Regression on treatment indicator",
                   "Regression on treatment indicator,\ncontrolling for pre-test",
                   NA)
# very good graph comparisons-not all graphs are duplicated here.
