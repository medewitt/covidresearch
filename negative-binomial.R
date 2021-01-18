library(cmdstanr)

r0 <- 1.5
phi_real <- .2

dat <- rnbinom(n = 100,  size = phi_real, mu = r0) 
hist(dat)

stan_dat <- list(
	N = length(dat),
	y = dat,
	pmf_support =10
)

mod <- cmdstan_model("nb.stan")

fit <- mod$sample(stan_dat, max_treedepth = 14, 
									parallel_chains = 2,adapt_delta =.98 )
fit$draws(variables = "phi") %>% 
	hist(col = "grey80")
fit$summary()

par(mfrow=c(1,2))
fit$draws(variables = "gini_out") %>% 
	hist(col = "grey80", main = "Gini Coefficient")
fit$draws(variables = "phi") %>% 
	hist(col = "grey80", main = "Overdispersion")
abline(v = phi_real, col = "red")
library(tidybayes)
tidy_draws.CmdStanMCMC <- function(model, ...) {
	return(posterior::as_draws_df(model$draws()))
}

tidy_draws(fit, gini[i])
# rstan::expose_stan_functions("nb.stan")
# 
# fit$summary("pmf")
# gini(pmf = rep(.01,100))
# 
# pmf <- c(.1,.2,.4,.5)
# gini(pmf)
# gini_coeff(df = data.frame(pmf = pmf, x=1:4))
