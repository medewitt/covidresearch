functions {
	real growth_model (real t, real h, real growth, real decay, real baseline){
		real value;
		if(t < h){
			value = baseline * exp (t * growth);
		} else {
			value = baseline * exp(growth * h - (t-h) * decay);
		}
		
		return value;
	}
}

data {
	int N;
	vector [N] y; 
	vector [N] t;
	real baseline;
	real prior_h;
}
transformed data{
	real<lower=0> sigma = .5;
}
parameters{
	real<lower=0> h;
	real<lower=0> growth;
	real<lower=0> decay;
	
}
transformed parameters{
	vector [N] mu;
	
	for(i in 1:N){
		mu[i] = growth_model(t[i], h, growth, decay, baseline);
	}
	
 	
}
model{
	growth ~ gamma(.2,.4);
	decay ~ gamma(.2,.2);
	h ~ normal(prior_h, 2);
	
	y ~ normal(mu, sigma);
}
generated quantities{
	vector[N] yhat;
	
	for( i in 1:N){
		yhat[i] = normal_rng(mu[i], sigma);
	}
	
	
}

