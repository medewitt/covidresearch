//Modified Gompertz Hierarchical Model From:
// https://discourse.mc-stan.org/t/hierarchical-gompertz-model/13724/60

data {
  int<lower=1> N;  // number of observations
  vector[N] Y;    // dependent variable
  vector[N] X;    // independent variable
  int<lower=1> N_group;   // number of groups
  int groups[N];  //  group assigments
  int N_pred;
  vector[N_pred] X_pred;
  int groups_pred[N_pred];
  vector[N_group] population_prior ;
}

parameters {
  vector<lower=0> [N_group] k  ;  // population-level effects
  real delay;  // population-level effects

  vector[N_group] A_group; // Vector of group effects

  real <lower=0> sigma;  // residual SD
}

transformed parameters{
  vector[N] mu; // population mean

  for (n in 1:N) {
    // initialize linear predictor term
    // compute non-linear predictor value
    mu[n] = A_group[groups[n]] * exp( -exp( -(k[groups[n]] * (X[n] - delay))) );
  }
}

model {

  // priors
  for(i in 1:N_group){
      A_group[i] ~ normal(population_prior[i], 10000);
  }
  
  k ~ normal(0, 20);
  delay ~ normal(0, 20);
  sigma ~ student_t(3, 0, 100);


  // likelihood
  Y ~ normal(mu, sigma);

}
generated quantities {
  real y_pred [N_pred] ;

  {
    vector[N_pred] mu_pred;
    for (n in 1:N_pred) {
    // compute non-linear predictor value
    mu_pred[n] = A_group[groups_pred[n]] * exp( -exp( -(k[groups_pred[n]] * (X_pred[n] - delay))) );

  }
  y_pred = normal_rng(mu_pred, sigma);
  }
}
