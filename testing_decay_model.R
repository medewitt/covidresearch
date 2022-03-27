a0 <- 1

exp_growth <- function(x, a0, g_rate){
	a0 * exp(g_rate * t)
}

test_fun <- function(t, h, init, growth, decay){
	if(t < h){
		a0 * exp (t * growth)
	} else {
		a0 * exp(h*growth - decay * (t-h))
	}
}


plot(vapply(1:50, function(x) test_fun(x, 20,1,  .3, .05), FUN.VALUE = numeric(1)))

library(cmdstanr)

mod <- cmdstan_model("growth-decay.stan")
n_use <- 200
y_true <- vapply(1:n_use, function(x) test_fun(x, 20,1,  .3, .05), FUN.VALUE = numeric(1))

y_obs <- rnorm(n_use,y_true,.5)

y_obs[y_obs<0] <- 0

stan_dat <- list(
	N = length(y_true),
	y = y_obs,
	t = 1:n_use,
	baseline = 1,
	prior_h = 20
)

fit <- mod$sample(stan_dat, adapt_delta = .95, max_treedepth = 14, iter_sampling = 4000)

fit$cmdstan_diagnose()

 
fit$summary()

plot(y_true, type= "l")
lines(fit$summary(variables = "yhat")$mean, col = "firebrick")
