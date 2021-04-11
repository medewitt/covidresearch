// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
  vector [N] week_no;
  real period;
  int weeks;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.

parameters {
  
  real<lower=0> sigma;
  real b0;
  real b1;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[N] mu;
  //
  // y = b0 + b1 * sin(2 * pi*t /period)
  mu = b0 + b1 * sin(2 * pi() * week_no/period);
  
  b0 ~ normal(1,1);
  b1 ~ normal(.1,1);
  
  y ~ normal(mu, sigma);
}

generated quantities {
  real y_hat [N];
  vector[N] mu;
  //
  // y = b0 + b1 * sin(2 * pi*t /period)
  mu = b0 + b1 * sin(2 * pi() * week_no/period);
  
  y_hat = normal_rng(mu, sigma);
}