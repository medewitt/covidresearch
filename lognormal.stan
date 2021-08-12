data{
	int N;
	vector [N] LOS;
}
parameters{
	real<lower=0> log_mu;
	real<lower=0> log_sd;
}
transformed parameters{
	real<lower=0> gamma = 1/exp(log_mu);
}
model{
	LOS ~ lognormal(log_mu,log_sd);
}
