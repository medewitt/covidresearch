library(tidyverse)
n.sim <- 1000
lambda <- 8/56 # 8 nursing homes in 45 days

diff.Date(c(as.Date("2020-03-18"), Sys.Date()))

generate_convolution <- function(n_sim=1000, lambda = 8/56, mu = 40, 
																 sigma = 10, alpa_tol = .05, 
																 covar = .3, 
																 severity_func = "gamma", 
																 severity_cap = 100){
	
	severity_function <- match.arg(severity_func, c("gamma", "pareto", "weibull"))
	if(severity_func=="gamma"){
		# Severity
		sigma.sq <- sigma^2
		beta <- mu/sigma.sq
		alpha <- beta * mu
		severity <- rgamma(n_sim, alpha, beta)
	} else if (severity_func =="weibull"){
		
		k <- (sigma/mu)^(-1.086)
		c <- mu/(gamma(1+1/k))
		severity <- rweibull(n_sim,k,c)
		
	} else{
		
		severity <- rmutil::rpareto(n = n_sim, m = mu, s = 100000)
		
		severity[severity>severity_cap] <- severity_cap
	}
	
	
	# Frequency
	frequency <- rpois(n_sim, lambda)
	
	frequency <- frequency * (1+covar)*rbinom(n_sim, 1, covar)
	
	# Convolution
	loss <- rpois(n_sim, severity * lambda)
	
	list(loss = loss, 
			 severity = severity, 
			 frequency = frequency)
}


# gamma severity function -------------------------------------------------

loss <- generate_convolution(lambda = 8/56, covar = 0)
op <-par(mgp=c(1.5,.5, 0), tck=-.01, 
				 mar=c(4,4,3,3), col.main = "#012169", col.sub = "#012169",
				 col.axis = "grey20", col.lab = "grey20", fg = "grey20")
op
par(mfrow = c(3,1))
summary(loss[["loss"]])
hist(loss[["loss"]], breaks = 30, right = FALSE,
		 col = "#00A2C2", 
		 xlab = "Number of Patients",
		 main = "Likely Positive Congregate Living Residents per Day", 
		 adj=0, xaxs="i", xaxt="n", bty="l")
axis(1, seq(0,40,2), cex.axis=1.1, cex.lab=1.1)
mtext(text = "Using a Gamma Outbreak Function", side = 3, adj = 0, col = "#012169")


# pareto severity function ------------------------------------------------

loss <- generate_convolution(lambda = 8/56, covar = 0, severity_func = "pareto")

summary(loss[["loss"]])
summary(loss[["severity"]])
hist(loss[["loss"]], breaks = 30, right = FALSE,
		 col = "#00A2C2", 
		 xlab = "Number of Patients",
		 main = "Likely Positive Congregate Living Residents per Day", 
		 adj=0, xaxs="i", xaxt="n", bty="l")
axis(1, seq(0,40,2), cex.axis=1.1, cex.lab=1.1)
mtext(text = "Using a Pareto Outbreak Function", side = 3, adj = 0, col = "#012169")


# weibull -----------------------------------------------------------------
loss <- generate_convolution(lambda = 8/56, covar = 0, 
														 severity_func = "weibull")

summary(loss[["loss"]])
summary(loss[["severity"]])
hist(loss[["loss"]], breaks = 30, right = FALSE,
		 col = "#00A2C2", 
		 xlab = "Number of Patients",
		 main = "Likely Positive Congregate Living Residents per Day", 
		 adj=0, xaxs="i", xaxt="n", bty="l")
axis(1, seq(0,40,2), cex.axis=1.1, cex.lab=1.1)
title(main = "", sub = "Yes")
#mtext(text = "Using a Weibull Outbreak Function", side = 3, adj = 0, col = "#012169")

