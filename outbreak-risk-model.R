library(tidyverse)
n.sim <- 1000
lambda <- 8/56 # 8 nursing homes in 45 days

diff.Date(c(as.Date("2020-03-18"), Sys.Date()))

generate_convolution <- function(n_sim=1000, lambda = 8/56, mu = 40, 
																 sigma = 10, alpa_tol = .05, 
																 covar = .3, 
																 severity_func = "gamma", severity_cap = 100){
	
	severity_function <- match.arg(severity_func, c("gamma", "pareto"))
	if(severity_func=="gamma"){
		# Severity
		sigma.sq <- sigma^2
		beta <- mu/sigma.sq
		alpha <- beta * mu
		severity <- rgamma(n_sim, alpha, beta)
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

summary(loss[["loss"]])
hist(loss[["loss"]], breaks = 30, right = FALSE,
		 col = "#00A2C2", 
		 xlab = "Numer of Patients",
		 main = "Likely Positive Congregate Living Residents per Day", 
		 adj=0, xaxs="i", xaxt="n", bty="l")
axis(1, seq(0,40,2), cex.axis=1.1, cex.lab=1.1)
mtext(text = "Using a Gamma Outbreak Function", side = 3, adj = 0)


# pareto severity function ------------------------------------------------

loss <- generate_convolution(lambda = 8/56, covar = 0, severity_func = "pareto")

summary(loss[["loss"]])
summary(loss[["severity"]])
hist(loss[["loss"]], breaks = 30, right = FALSE,
		 col = "#00A2C2", 
		 xlab = "Numer of Patients",
		 main = "Likely Positive Congregate Living Residents per Day", 
		 adj=0, xaxs="i", xaxt="n", bty="l")
axis(1, seq(0,40,2), cex.axis=1.1, cex.lab=1.1)
mtext(text = "Using a Pareto Outbreak Function", side = 3, adj = 0)

k <- (sd(rw)/mean(rw))^(-1.086)
c <- mean(rw)/(gamma(1+1/k))

k <- (20/mean(40))^(-1.086)
c <- mean(40)/(gamma(1+1/k))
hist(rweibull(100,k,c))
