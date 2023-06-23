
install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking",ref="Experimental")
remotes::install_github("stan-dev/cmdstanr")


install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
devtools::install_github("rmcelreath/rethinking")
library(rethinking)

remove.packages("")

# https://github.com/rmcelreath/rethinking#installation
# this version works, 'slim' version
install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
devtools::install_github("rmcelreath/rethinking@slim")

library(rethinking)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

fit <- quap(
  f ,
  data=list(y=c(-1,1)) ,
  start=list(mu=0,sigma=1)
)
summary(fit)
