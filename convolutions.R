
# Forward Convolve
y <- rpois(10, 1)
theta <- c(.1,.2,.2,.5)
out2 <- round(nccovid::epi_convolve(y, theta))
cbind(out1, y)
sum(out1)
sum(y)
sum(out2)

convolve2 <- function(x,pmf){
	
	pmf_n <- length(pmf)
	
	x_n <- length(x)
	
	out_length <- x_n + pmf_n - 1
	
	pmf_hat <- c(pmf, rep(0,out_length-pmf_n))
	
	x_hat <- c(x, rep(0,out_length-x_n))
	
	convolve(x_hat, pmf_hat)
}


deconvolve2 <- function(x,pmf){
	
	pmf_n <- length(pmf)
	
	x_n <- length(x)
	
	out_length <- x_n + pmf_n - 1
	
	pmf_hat <- c(rep(0,out_length-pmf_n), pmf )
	
	x_hat <- c(rep(0,out_length-x_n),x)
	
	convolve(x_hat, pmf_hat)
}

cv <- convolve2(y, theta)
plot(y, type = "b", ylim=c(0,5), xlim = c(0,15))
lines(round(cv), col = "red")
sum(y)
sum(cv)

# Deconvole
cv <- convolve2(y, rev(theta))
plot(y, type = "b", ylim=c(0,5), xlim = c(0,15))
lines(round(cv), col = "red")

