r0 <- seq(1,3,.5)


s <- 10000

prop_vac <- seq(0,.5,.1)

at_peak <- 1/r0

prop_infection_peak <- function(r0, prop_vac){
	
	new_r0 <- r0 * (1-prop_vac)
	
	frac_infected <- 1-1/new_r0
	
	frac_infected
	
}
prop_infection_peak(5,.5)

risk_ratio <- .1

# Optimise -rP - pi_theta (1-p) 
# vaccinate at Prob P where
# r is risk ratio of perceived risk
# pi_theta is infection risk at fraction theta vaccinated

payoff_function <- function(r, P, pi_theta, ){
	
}