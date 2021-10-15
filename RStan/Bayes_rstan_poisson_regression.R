
# Poisson regression in Stan ----------------------------------------------
# https://rpubs.com/kaz_yos/stan-pois1

library(ISwR)
library(rstan)
library(tableone)
# data 
data("eba1977")
summary(eba1977)

# Regular Poisson model
glm1 <- glm(formula = cases ~ age + city + offset(log(pop)),
            family  = poisson(link = "log"),
            data    = eba1977)
summary(glm1)

# using rstan
# Model matrix
modMat <- as.data.frame(model.matrix(glm1))
modMat$offset <- log(eba1977$pop)
names(modMat) <- c("intercept", "age55_59", "age60_64", "age65_69", "age70_74", 
                   "age75plus", "cityHorsens", "cityKolding", "cityVejle", 
                   "offset")

dat   <- as.list(modMat)
dat$y <- eba1977$cases
dat$N <- nrow(modMat)
dat$p <- ncol(modMat) - 1

# stan model code
# Poisson model example

model_poisson ="
data {
  // Define variables in data
  // Number of observations (an integer)
  int<lower=0> N;
  // Number of beta parameters
  int<lower=0> p;

  // Covariates
  int <lower=0, upper=1> intercept[N];
  int <lower=0, upper=1> age55_59[N];
  int <lower=0, upper=1> age60_64[N];
  int <lower=0, upper=1> age65_69[N];
  int <lower=0, upper=1> age70_74[N];
  int <lower=0, upper=1> age75plus[N];
  int <lower=0, upper=1> cityHorsens[N];
  int <lower=0, upper=1> cityKolding[N];
  int <lower=0, upper=1> cityVejle[N];

  // offset
  real offset[N];

  // Count outcome
  int<lower=0> y[N];
}

parameters {
  // Define parameters to estimate
  real beta[p];
}

transformed parameters  {
  //
  real lp[N];
  real <lower=0> mu[N];

  for (i in 1:N) {
    // Linear predictor
    lp[i] <- beta[1] + beta[2]*age55_59[i] + beta[3]*age60_64[i] + beta[4]*age65_69[i] + beta[5]*age70_74[i] + beta[6]*age75plus[i]+ beta[7]*cityHorsens[i] + beta[8]*cityKolding[i] + beta[9]*cityVejle[i] + offset[i];

    // Mean
    mu[i] <- exp(lp[i]);
  }
}

model {
  // Prior part of Bayesian inference
  // Flat prior for mu (no need to specify if non-informative)


  // Likelihood part of Bayesian inference
  y ~ poisson(mu);
}
"

# stan model in R
resStan <- stan(model_code = model_poisson, data = dat,
                chains = 4, iter = 5000, warmup = 1000, thin = 10)

# Show traceplot
traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)

# Comparison
# Frequentist
tableone::ShowRegTable(glm1, exp = FALSE)

# Bayesian
print(resStan, pars = c("beta"))
