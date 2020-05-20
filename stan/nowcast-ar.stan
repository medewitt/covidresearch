// From https://mc-stan.org/docs/2_18/stan-users-guide/autoregressive-section.html
// Allowing for flexible autoregression for time series modeling

data {
  int<lower=0> K;  // Order of Autoregression
  int<lower=0> N1; // number of low frequency observations
  int<lower=0> N2; // number of high frequency observations
  real y[N1];      // outcome
  real z[N2];      // predictors
  real start_value;
  int freq;
}
parameters {
  real alpha;
  real<lower = 0> sigma_epsilon;
  real beta[K];
  real sigma_eta;
  vector[N2] x;
}

model {
	int count;
  count = 0;
  // priors
  sigma_epsilon ~ cauchy(0,1);
  sigma_eta ~ normal(0,1);
  beta ~ normal(0,1);
  alpha~normal(start_value,1);
  // likelihood
  
  for (n in (K+1):N2) {
    real mu = alpha;
    for (k in 1:K)
      mu += beta[k] * z[n-k];
    x[n] ~ normal(mu+x[n-1], sigma_eta);
    
    if(n%freq==0){
      count = count + 1;
      
      if(count <= N1){
      	y[count] ~normal(x[n], sigma_epsilon);
      }
    }
  }
}
