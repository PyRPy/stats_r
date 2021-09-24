// Full Stan program for the linear model
// https://www.r-bloggers.com/2020/01/applied-bayesian-statistics-using-stan-and-r/

data {
  int<lower=1> N; // num. observations
  int<lower=1> K; // num. predictors
  matrix[N,K] x; // model matrix
  vector[N] y;    // outcome vector
}

parameters {
  vector[K] beta;      // coef vector
  real<lower=0> sigma; // scale parameter
}

transformed parameters {
  vector[N] mu;  // declare lin. pred.
  mu = x * beta; // assign lin. pred.
}

model {
  // priors
  beta ~ normal(0, 10);  // priors for beta
  sigma ~ cauchy(0, 5);  // prior for sigma
  
  // log-likelihood
  target += normal_lpdf(y | mu, sigma);
}