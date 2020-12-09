r0 <- seq(1,3,.5)


s <- 10000

prop_vac <- seq(0,.9,.01)

at_peak <- 1/r0

prop_infection_peak <- function(r0, prop_vac){
	
	new_r0 <- r0 * (1-prop_vac)
	
	frac_infected <- 1-1/new_r0
	
	frac_infected[frac_infected<0]<-0
	
	frac_infected
}
prop_infection_peak(5,.5)

risk_ratio <- .01/.001

hht <- function(r0){
	1-1/r0
}

# Optimise -rP - pi_theta (1-p) 
# vaccinate at Prob P where
# r is risk ratio of perceived risk
# pi_theta is infection risk at fraction theta vaccinated

plot(prop_vac, prop_infection_peak(r0 = 3.5, prop_vac =prop_vac ),type = "l",
		 ylab = expression(P(Infection)),
		 xlab = "Proportion Vaccinated")
title(expression("Probability of Infection Given Vaccination"~ R[0]~of~"3.5",
								 adj=1))

payoff_function <- function(r, P, pi_theta){
	-r * P - (pi_theta * (1-P))
}

plot(prop_vac, payoff_function(r = .9,P = prop_vac,
										 pi_theta = prop_infection_peak(2.5,prop_vac)))
abline(v = hht(2.5))

p <- .1
out <- vector(mode = "numeric")
risk <- seq(.1,3,by = .01)

out_matrix <- matrix(data = NA, nrow = length(risk), ncol = length(prop_vac))
for(i in seq_along(risk)){
	for(j in seq_along(prop_vac)){
		out_matrix[i,j]<-payoff_function(r = risk[i],P = prop_vac[j],
														pi_theta = prop_infection_peak(2.5,prop_vac[j]))
	}
}
hht(2.5)

rownames(out_matrix) <- risk
colnames(out_matrix) <- prop_vac
filled.contour(out_matrix, xlab = "Risk", ylab = "Prop Vaccinated")
contour(out_matrix, add = TRUE)
abline(h = hht(2.5), lty = 3)
title(expression("Vaccine Game Expected Utility Given"~R[0]~of~2.5))
plot(prop_vac,out)
abline(v = hht(3))
abline(h = max(out), lty = 3)
