//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> N1;  // number of observations (group 1)
  int<lower=0> N2;  // number of observations (group 2)
  vector[N1] y1;  // response time (group 1);
  vector[N2] y2;  // response time (group 2);
}
parameters {
  real<lower=0> mu_1;  // mean of group 1
  real beta;  // difference in means
  real<lower=0> sigma;  // pooled standard deviation
}
transformed parameters {
  real<lower=0> mu_2 = mu_1 + beta; 
}
model {
  y1 ~ normal(mu_1, sigma);
  y2 ~ normal(mu_2, sigma);
  // prior
  mu_1 ~ normal(0.5, 2.5);
  beta ~ normal(0, 2.5);
  sigma ~ student_t(4, 0, 1);
}
generated quantities {
  real y1rep[N1];
  real y2rep[N2];
  for (i in 1:N1) {
    y1rep[i] = normal_rng(mu_1, sigma);
  }
  for (i in 1:N2) {
    y2rep[i] = normal_rng(mu_2, sigma);
  }
}