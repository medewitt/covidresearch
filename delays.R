lab_results <- c(
	1.4,
	1.4,
	1.4,
	1.6,
	1.8,
	1.5,
	1.2,
	1.3,
	1.3,
	1.4,
	1.6,
	1.8,
	1.6,
	1.4,
		1.5,
	1.7,
	1.7,
	1.8,
	1.9,
	2,
	1.5,
	1.6,
	1.6,
	1.6,
	1.9,
	2.2
)

electronic_report <- c(
	.9,
	1,
	.8,
	.6,
	.7,
	.9,
	3.2,
	.8,
	.8,
	.8,
	.7,
	.8,
	1.3,
	1,
	.5,
	1.1,
	.8,
	.8,
	.8,
	4.6,
	3.5,
	.7,
	.8,
	.8,
	.8,
	.6
)
combined_delay <- lab_results+electronic_report

fitdistrplus::fitdist(data = combined_delay,distr = "gamma")
fitted <- distcrete::distcrete(name = "gamma", shape = 11.37, rate = 4.11, 
															 interval =1:10,w = 1)
hist(fitted$r(100))
hist(combined_delay)
EpiNow2::bootstrapped_dist_fit(combined_delay, dist = "gamma")
