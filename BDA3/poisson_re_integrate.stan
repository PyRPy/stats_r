// Poisson regression with hierarchical intercept ("random effect")
functions {
  real integrand(real z, real notused, array[] real theta,
               array[] real Xi, array[] int yi) {
    real sigmaz = theta[1];
    real offsetti_plus_alpha = theta[2];
    int ntheta = num_elements(theta);
    vector[ntheta-2] beta = to_vector(theta[3:ntheta]);
    return exp(normal_lpdf(z|0,sigmaz)+poisson_log_glm_lpmf(yi | to_row_vector(Xi), z+offsetti_plus_alpha, beta));
  }
}
data {
  int<lower=0> N;           // number of data points
  int<lower=0> P;           // number of covariates
  matrix[N,P] X;            // covariates
  array[N] int<lower=0> y;  // target
  vector[N] offsett;        // offset (offset variable name is reserved)
}
parameters {
  real alpha;               // intercept
  vector[P] beta;           // slope
  vector[N] z;              // individual intercept ("random effect")
  real<lower=0> sigmaz;     // prior scale for z
}
model {
  // priors
  alpha ~ normal(0, 3);  
  beta ~ normal(0, 3);    
  z ~ normal(0, sigmaz);  
  sigmaz ~ normal(0, 1);
  // observation model
  y ~ poisson_log_glm(X, z+offsett+alpha, beta); 
}
generated quantities {
  // log_lik for PSIS-LOO
  vector[N] log_lik;
  for (i in 1:N) {
    // z as posterior draws, this would be challenging for PSIS-LOO (and WAIC)
    // log_lik[i] = poisson_log_glm_lpmf({y[i]} | X[i,], z[i]+offsett[i]+alpha, beta);
    // we can integrate each z[i] iut with 1D adaptive quadrature 
    log_lik[i] = log(integrate_1d(integrand,
                              negative_infinity(),
                              positive_infinity(),
                              append_array({sigmaz}, append_array({offsett[i]+alpha}, to_array_1d(beta))),
                  to_array_1d(X[i,]),
                  {y[i]}));
  }
}