library(EpiModel)

param <- param.dcm(inf.prob = 0.05, # Prob of Infection
									 act.rate = 10,  # Acts per Day
									 rec.rate = 1/14, # inverse disease rate
									 a.rate = 1/95,  # Arrival Rate
									 ds.rate = 1/100, 
									 di.rate = 1/80, 
									 dr.rate = 1/100)
init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
mod <- dcm(param, init, control)

par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))
plot(mod, popfrac = TRUE, alpha = 0.5,
		 lwd = 4, main = "Compartment Sizes")
plot(mod, y = "si.flow", lwd = 4, col = "firebrick",
		 main = "Disease Incidence", legend = "n")
par(mfrow = c(1, 1))

comp_plot(mod, at = 30, digits = 1)


# SEIR --------------------------------------------------------------------

SEIR <- function(t, t0, parms) {
	with(as.list(c(t0, parms)), {
		
		# Population size
		num <- s.num + e.num + i.num + r.num
		
		# Effective contact rate and FOI from a rearrangement of Beta * c * D
		ce <- R0 / i.dur
		lambda <- ce * i.num/num
		
		dS <- -lambda*s.num
		dE <- lambda*s.num - (1/e.dur)*e.num
		dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num
		dR <- (1 - cfr)*(1/i.dur)*i.num
		
		# Compartments and flows are part of the derivative vector
		# Other calculations to be output are outside the vector, but within the containing list
		list(c(dS, dE, dI, dR, 
					 se.flow = lambda * s.num,
					 ei.flow = (1/e.dur) * e.num,
					 ir.flow = (1 - cfr)*(1/i.dur) * i.num,
					 d.flow = cfr*(1/i.dur)*i.num),
				 num = num,
				 i.prev = i.num / num,
				 ei.prev = (e.num + i.num)/num)
	})
}

param <- param.dcm(R0 = 2.5, # Reproductive Rate
									 e.dur = 10,  # Duration of Exposed State
									 i.dur = 14,  # Duration of Infection
									 cfr = c(0.01, 0.015, 0.02) # Proportion of Infected who die
									 )
init <- init.dcm(s.num = 1e6, 
								 e.num = 10, 
								 i.num = 0, 
								 r.num = 0,
								 se.flow = 0, 
								 ei.flow = 0, 
								 ir.flow = 0, 
								 d.flow = 0)
control <- control.dcm(nsteps = 500, dt = 1, new.mod = SEIR)
mod <- dcm(param, init, control)
mod
par(mfrow = c(1, 2))
plot(mod, y = "i.num", run = 2, main = "Prevalence")
plot(mod, y = "se.flow", run = 2, main = "Incidence")

par(mfrow = c(1, 2))
plot(mod, y = "i.num", main = "Number Infected")
plot(mod, y = "i.prev", main = "Percent Infected", ylim = c(0, 0.5), legend = "full")
