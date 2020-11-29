//https://github.com/stan-dev/example-models/blob/master/knitr/convert-odes/sir.stan

functions {
  vector simple_SIR(real t,
                    vector y,
                    real beta,    //  contact rate
                    real gamma) { // bacteria removal rate
    vector[3] dydt;

    dydt[1] = -beta * y[1] *y[2]/sum(y);
    dydt[2] = beta * y[1] *y[2]/sum(y) - gamma * y[2];
    dydt[3] = gamma * y[2];

    return dydt;
  }
}


data {
  int<lower=0> N_t;
  real t[N_t];
  vector[3] y0;
  int cases[N_t-1];
  int pred_window;
  real t_pred[N_t+pred_window];
}

transformed data {
  real t0 = 0;
}

parameters {
  real<lower=0> beta;
  real<lower=0> gamma;
  real<lower=0> phi_inv;
}

transformed parameters {
  real<lower=0> phi = 1. / phi_inv;
  real incidence[N_t-1];
  vector<lower=0>[3] y[N_t] = ode_rk45_tol(simple_SIR, y0, t0, t,
					   1e-6, 1e-6, 1000,
					   beta, gamma);
	for (i in 1:(N_t-1))
  	incidence[i] = y[i, 1] - y[i + 1, 1];
}
  
model {
  
  cases ~ neg_binomial_2(incidence, phi);
  beta ~ cauchy(0, 2.5);
  gamma ~ cauchy(0, 1);
  phi_inv ~ exponential(1);

}
generated quantities{
  real incidence_out[N_t+pred_window-1];
  real reff[N_t+pred_window-1];
  real r0;
  real recovery;
  vector<lower=0>[3] y_pred[N_t+pred_window] = ode_rk45_tol(simple_SIR, y0, t0, t_pred,
					   1e-6, 1e-6, 1000,
					   beta, gamma);
	for(i in 1:(N_t+pred_window-1)){
	  incidence_out[i] = y_pred[i,1] - y_pred[i+1,1];
	}
	
	for(i in 1:(N_t+pred_window-1)){
	  reff[i] = beta/gamma * y_pred[i,1]/sum(y_pred[i,]);
	}
	
	r0 = beta/gamma;
	recovery = 1/gamma;
					   
}
