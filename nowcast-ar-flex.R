library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

model_ar <- stan_model("stan/nowcast-ar.stan")

# Fake Data----
z <-arima.sim(n = 300, model = list(ar = c(.5, .05, 0, .2,.01,.01)))
plot.ts(z)
n <- 300
freq <- 3
x <- rep(NA, n)
x[1] <- 100
for(i in 2:n){
	x[i] <- x[i-1] +z[i] + rnorm(1, 0, 0.5)
}
plot(x)

y <- x + rnorm(n, 0, 1)

y[!(1:n%%freq==0)] <- NA

y[((length(y)-6):length(y))] <- NA
dat <-data.frame(y, z) 

model_list <- list(N1 = length(dat$y[!is.na(dat$y)]), 
									 N2 = n, 
									 K = 4L,
									 freq = freq, 
									 start_value = dat$y[!is.na(dat$y)][1],
									 y = dat$y[!is.na(dat$y)], 
									 z= unlist(z))

fit <- sampling(model_ar, model_list, iter = 2000, 
								chains = 2, refresh = 0,
								control = list(adapt_delta = .95,
															 max_treedepth = 15))

# diagnostsic -------------------------------------------------------------
pairs(fit, pars = "beta", "alpha")
rstan::check_hmc_diagnostics(fit)

# inference ---------------------------------------------------------------
stan_plot(fit, pars = c("beta", "sigma_eta"))
print(fit, pars = c("beta", "sigma_eta", "sigma_epsilon"))

x_mod <- extract(fit, pars = "x", permuted = F)
x_mod <- plyr::adply(x_mod, 2)

# Summarise the parameters
library(dplyr)
yy <- dat$y
x_summarise <- x_mod %>% 
	dplyr::select(-chains) %>% 
	tidyr::gather(variable, value) %>%
	mutate(obs = stringr::str_extract(variable, "[0-9]{1,4}") %>% as.numeric) %>%
	group_by(obs) %>%
	summarise(Median = median(value),
						Lower = quantile(value, 0.025),
						Upper = quantile(value, 0.975)) %>%
	mutate(Actual = x,
				 Signal = yy) #%>% 
	# mutate(Median = lead(Median,3),
	# 			 Lower = lead(Lower,3),
	# 			 Upper = lead(Upper,3))

mean(x_summarise$Actual<x_summarise$Lower | x_summarise$Actual>x_summarise$Upper, na.rm = T) %>% 
	scales::percent()

library(ggplot2)
x_summarise[-c(1:4),]%>% 
	ggplot(aes(x = obs)) +
	#geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "orange", alpha = 0.5) +
	geom_line(aes(y = Median)) +
	geom_line(aes(y = Actual), colour = "red") +
	geom_point(aes(y = Signal), size = 2, color = "blue") +
	labs(title = "Points are low-frequency observations\nRed is actual underlying (hf) series\nblack and orange are our estimate bounds")+
	theme_minimal()+
	ylim(50,200)
