// this example follows the scripts from the following post on r-bloggers
// https://www.r-bloggers.com/2022/08/robservations-36-opinions-on-rstudios-name-change-a-bayesian-approach-with-stan/

data{
  // Total number of votes
  int n;
  // number individual votes
  // Love it
  int y1[n];
  // Hate it
  int y2[n];
  // Somewhere in between
  int y3[n];

}

parameters{
  // Prior Parameters
  real<lower=0, upper=1> theta1;
  real<lower=0, upper=1> theta2;
  real<lower=0, upper=1> theta3;
}

// Prior distribution
model{
  // All have uniform priors
  theta1 ~ beta(1,1);
  theta2 ~ beta(1,1);
  theta3 ~ beta(1,1);

  // Likelihood Functions
  y1~ bernoulli(theta1);
  y2~ bernoulli(theta2);
  y3~ bernoulli(theta3);
}