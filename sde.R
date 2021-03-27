#https://github.com/TheoMichelot/smoothSDE?s=03
devtools::install_github("TheoMichelot/smoothSDE")
library(smoothSDE)
n <- 1000
times <- 1:n
mu <- rep(0.1, n)
sigma <- exp(cos(2*pi*times/500))
dZ <- rnorm(n - 1, mean = mu[-n], sd = sigma[-n])
Z <- cumsum(c(0, dZ)) + rpois(n, abs(rnorm(n)))

data <- data.frame(ID = 1,
									 Z = Z,
									 time = times)
ggplot(data, aes(time, Z)) + geom_line()

formulas <- list(mu = ~1,
								 sigma = ~ s(time, k = 10, bs = "ts"))

bm <- SDE$new(formulas = formulas,
							data = data,
							type = "BM",
							response = "Z")

bm$fit()
							
bm$plot_par("time", n_post = 100)
							