
# Basic Markov Chain simulation -------------------------------------------


# Gibbs sampling ----------------------------------------------------------
# https://avehtari.github.io/BDA_R_demos/demos_ch11/demo11_1.html

library(ggplot2)
theme_set(theme_minimal())
library(tidyr)
library(gganimate)
library(MASS)
library(posterior)
library(rprojroot)

# bivariate normal distribution
y1 <- 0
y2 <- 0
r <- 0.8
Sigma <- diag(2) # construct identity matrix
Sigma[1, 2] <- r
Sigma[2, 1] <- r

#  90% HPD interval
dft <- data.frame(mvrnorm(100000, c(0, 0), Sigma))

# Starting value of the chain
t1 <- -2.5
t2 <- 2.5

# Number of iterations.
M <- 2*2500

# Allocate memory for the sample
tt <- matrix(rep(0, 2*M), ncol = 2)
tt[1,] <- c(t1, t2)    # Save starting point

# For demonstration load pre-computed values
# Replace this with your algorithm!
# tt is a M x 2 array, with M draws of both theta_1 and theta_2
load("BDA3_Data/demo11_1.RData")

# first 50 draws
df100 <- data.frame(id=rep(1,100),
                    iter=1:100,
                    th1 = tt[1:100, 1],
                    th2 = tt[1:100, 2],
                    th1l = c(tt[1, 1], tt[1:(100-1), 1]),
                    th2l = c(tt[1, 2], tt[1:(100-1), 2]))

#  first 1000 observations
S <- 1000
dfs <- data.frame(th1 = tt[1:S, 1], th2 = tt[1:S, 2])

# Remove warm-up period of 50 first draws later
warm <- 50

# labels and frame indices for the plot
labs1 <- c('Draws', 'Steps of the sampler', '90% HPD')
ind1 <- (1:50)*2-1
df100s <- df100
df100s[ind1+1,3:4]=df100s[ind1,3:4]
p1 <- ggplot() +
  geom_point(data = df100s,
             aes(th1, th2, group=id, color ='1')) +
  geom_segment(data = df100, aes(x = th1, xend = th1l, color = '2',
                                 y = th2, yend = th2l)) +
  stat_ellipse(data = dft, aes(x = X1, y = X2, color = '3'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('red', 'forestgreen','blue'), labels = labs1) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA, NA), linetype = c(0, 1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

#  gif animation
anim <- animate(p1 +
                  transition_reveal(along=iter) +
                  shadow_trail(0.01))
# Show the animation
anim
p1


# Metropolis algorithm ----------------------------------------------------
# https://avehtari.github.io/BDA_R_demos/demos_ch11/demo11_2.html
# parameters are same as Gibbs

# Metropolis proposal distribution scale
sp <- 0.3

# 90% HPD interval
dft <- data.frame(mvrnorm(100000, c(0, 0), Sigma))

# starting value of the chain
t1 <- -2.5
t2 <- 2.5

# number of iterations
M = 5000
# Allocate memory for the sample
tt <- matrix(rep(0, 2*M), ncol = 2)
tt[1,] <- c(t1, t2)    # Save starting point
load("BDA3_Data/demo11_2a.RData")
df100 <- data.frame(id=rep(1,100),
                    iter=1:100,
                    th1 = tt[1:100, 1],
                    th2 = tt[1:100, 2],
                    th1l = c(tt[1, 1], tt[1:(100-1), 1]),
                    th2l = c(tt[1, 2], tt[1:(100-1), 2]))
# first 5000 observations after warmup of 50
S <- 5000
warm <- 500
dfs <- data.frame(th1 = tt[(warm+1):S, 1], th2 = tt[(warm+1):S, 2])

# Remove warm-up period of 50 first draws later
# labels and frame indices for the plot
labs1 <- c('Draws', 'Steps of the sampler', '90% HPD')
p1 <- ggplot() +
  geom_jitter(data = df100, width=0.05, height=0.05,
              aes(th1, th2, group=id, color ='1'), alpha=0.3) +
  geom_segment(data = df100, aes(x = th1, xend = th1l, color = '2',
                                 y = th2, yend = th2l)) +
  stat_ellipse(data = dft, aes(x = X1, y = X2, color = '3'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('red', 'forestgreen','blue'), labels = labs1) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA, NA), linetype = c(0, 1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

p1
anim <- animate(p1 +
                  transition_reveal(along=iter) + shadow_trail(0.01))

anim
p1
