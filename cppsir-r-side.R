params <- list()
params <- within(params, {
	
	## set rng state
	seed <- 0
	tau <- 0.001 # in years
	nyears <- 10
	
	## total number of steps
	nsteps <- nyears/tau
	
	mu <- 1/70 #death rate
	gamma <- 365/10 #recovery rate
	R0 <- 10
	## refers to R0 above
	beta <- R0*(gamma+mu) #transmission rate
	nu <- mu #birth rate
	
	## initial conditions, list within list
	## use within() to modify empty list, as above
	init <- within(list(), {
		pop <- 1e6
		S <- round(pop/R0)
		I <- round(pop*mu*(1-1/R0)/(gamma+mu))
		## refers to S,I above
		R <- pop-S-I
	})
})

tauleapCpp(params)

out <- purrr::map_dfr(100, ~tauleapCpp(params), .id = "sim")
