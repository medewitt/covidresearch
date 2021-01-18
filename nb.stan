functions{
	real gini (vector pmf){
		int support = num_elements(pmf);
		vector [support] support_val;
		real mu;
		real out;
		for(i in 1:support){
			support_val[i] = i;
		}
		
		mu = sum(support_val .* pmf);
		out = 0;
		for(i in 1:support){
			for(j in 1:support){
				out+=pmf[i]*pmf[j]*abs(i-j);
			}
		}
	
	return out/(2*mu);
	}
}

data{
	int<lower=0> N;
	int<lower=0> y [N];
	int pmf_support;
}
parameters{
	real<lower=0> phi_inv;
	real mu;
}
transformed parameters{
	real phi = 1./phi_inv;
}
model{
	// Priors
	phi_inv ~ exponential(1);
	
	// Model
	target += neg_binomial_2_lpmf(y | mu, phi);
} 
generated quantities{
	vector [pmf_support] pmf;
	real gini_out;
	pmf[1] = neg_binomial_2_cdf(1,mu, phi);
	for(i in 2:pmf_support){
		pmf[i] = neg_binomial_2_cdf(i,mu, phi) - neg_binomial_2_cdf(i-1,mu, phi);
	}
	
	gini_out = gini(pmf);
	
}